/// Refactoring (performance/idiom): a loop whose whole body is a single
/// ResizeArray Add is one AddRange call.
///
///     for x in xs do acc.Add x            →  acc.AddRange xs
///     for x in xs do acc.Add(x * 2)       →  acc.AddRange(xs |> Seq.map (fun x -> x * 2))
///
/// AddRange enumerates the source once and applies the projection in the
/// same order the loop did, so the rewrite is behavior-preserving; when the
/// source has a known count it also pre-sizes the backing array.
///
/// Safety rules:
///   - the loop body is exactly the Add call — any other statement means
///     the loop is not a pure accumulation and it is left alone
///   - `Add` must resolve (typed check results) to
///     System.Collections.Generic.List`1.Add: HashSet.Add and friends have
///     different semantics and often no AddRange
///   - source, receiver, and argument are single-line, and the argument is
///     safe to inline into a lambda body
///   - the file must have no type errors
module FSharp.Refactorings.AddRange

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

type Suggestion =
    { Range: range
      OriginalText: string
      ReplacementText: string }

/// `recv.Add arg` — the Add identifier, the receiver's text range end, and
/// the argument.
[<return: Struct>]
let private (|AddCall|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(
        isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) as recv; argExpr = arg) when
        ids.Length >= 2 && (List.last ids).idText = "Add" && isSingleLine recv.Range
        ->
        let addIdent = List.last ids

        let receiverRange =
            Range.mkRange
                recv.Range.FileName
                recv.Range.Start
                (Position.mkPos addIdent.idRange.StartLine (addIdent.idRange.StartColumn - 1))

        ValueSome(addIdent, receiverRange, arg)
    | SynExpr.App(
        isInfix = false
        funcExpr = SynExpr.DotGet(expr = receiver; longDotId = SynLongIdent(id = [ addIdent ]))
        argExpr = arg) when addIdent.idText = "Add" && isSingleLine receiver.Range ->
        ValueSome(addIdent, receiver.Range, arg)
    | _ -> ValueNone

/// Does the Add identifier resolve to List<'T>.Add?
let private resolvesToListAdd (check: FSharpCheckFileResults) (source: ISourceText) (addIdent: Ident) =
    let r = addIdent.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ addIdent.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value ->
            (try
                value.ApparentEnclosingEntity
                |> Option.bind (fun e -> e.TryFullName)
                |> Option.defaultValue ""
             with _ ->
                 "")
                .StartsWith
                "System.Collections.Generic.List`"
        | _ -> false
    | None -> false

/// A lambda-parameter rendering of the loop pattern.
let private lambdaPatText (source: ISourceText) (pat: SynPat) =
    let text = textOfRange source pat.Range

    match pat with
    | SynPat.Named _
    | SynPat.Wild _
    | SynPat.Paren _ -> Some text
    | SynPat.Tuple _ -> Some($"({text})")
    | _ -> None

/// Find accumulate-only loops over List<'T>. Requires typed check results.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.ForEach(pat = pat; enumExpr = enumExpr; bodyExpr = body) when isSingleLine enumExpr.Range ->
                  let body =
                      match body with
                      | SynExpr.Do(expr = inner) -> inner
                      | other -> other

                  match body with
                  | AddCall(addIdent, receiverRange, arg) when
                      isSingleLine arg.Range && resolvesToListAdd check source addIdent
                      ->
                      let receiverText = textOfRange source receiverRange
                      let element = stripParens arg

                      let replacement =
                          match element, pat with
                          | SynExpr.Ident v, SynPat.Named(ident = SynIdent(ident = loopVar)) when
                              v.idText = loopVar.idText
                              ->
                              Some(receiverText + ".AddRange " + argumentText source enumExpr)
                          | _ ->
                              match lambdaPatText source pat with
                              | Some patText when isSafeInline element && isSingleLine pat.Range ->
                                  Some(
                                      receiverText
                                      + ".AddRange("
                                      + atomicText source enumExpr
                                      + " |> Seq.map (fun "
                                      + patText
                                      + " -> "
                                      + textOfRange source element.Range
                                      + "))"
                                  )
                              | _ -> None

                      match replacement with
                      | Some replacementText ->
                          { Range = expr.Range
                            OriginalText = textOfRange source expr.Range
                            ReplacementText = replacementText }
                      | None -> ()
                  | _ -> ()
              | _ -> () ]
