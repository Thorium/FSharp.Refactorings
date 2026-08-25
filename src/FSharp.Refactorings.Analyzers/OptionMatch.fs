/// Refactoring: an IsSome test followed by .Value access is a pattern
/// match without the throwing accessor.
///
///     if x.IsSome then x.Value + 1 else 0
///         →  match x with | Some v -> v + 1 | None -> 0
///
/// `.Value` throws when the option is None; after the rewrite the value is
/// only in scope where it exists. The `IsNone` and `not x.IsSome` forms
/// swap branches, an else-less unit `if` gains `| None -> ()`, and a
/// ValueOption receiver spells the cases ValueSome/ValueNone.
///
/// Safety rules:
///   - the receiver is a plain identifier that resolves (typed check
///     results) to FSharp.Core's option or voption — a custom type with
///     its own IsSome/Value members never matches
///   - the whole `if` is single-line; the None-arm must not itself touch
///     `.Value` (that code throws today — not ours to rewrite), and the
///     Some-arm must use it at least once
///   - the binder name (`v`, falling back to `<x>Value`) must not appear
///     anywhere in the expression
module FSharp.Refactorings.OptionMatch

open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

type Suggestion =
    { Range: range
      OriginalText: string
      ReplacementText: string }

[<TailCall>]
let rec private stripAbbreviations (t: FSharpType) =
    if t.HasTypeDefinition && t.TypeDefinition.IsFSharpAbbreviation then
        stripAbbreviations t.TypeDefinition.AbbreviatedType
    else
        t

/// "Some"/"None" or "ValueSome"/"ValueNone" for the receiver's type.
let private caseNamesFor (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value ->
            try
                let t = stripAbbreviations value.FullType

                if not t.HasTypeDefinition then
                    None
                else
                    match t.TypeDefinition.TryFullName with
                    | Some name when name.StartsWith "Microsoft.FSharp.Core.FSharpOption`" -> Some("Some", "None")
                    | Some name when name.StartsWith "Microsoft.FSharp.Core.FSharpValueOption`" ->
                        Some("ValueSome", "ValueNone")
                    | _ -> None
            with _ ->
                None
        | _ -> None
    | None -> None

/// `x.IsSome` / `x.IsNone` / `not <either>` → (x, negated).
[<return: Struct>]
let private (|OptionTest|_|) (e: SynExpr) =
    match e with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ x; prop ])) when prop.idText = "IsSome" -> ValueSome(x, false)
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ x; prop ])) when prop.idText = "IsNone" -> ValueSome(x, true)
    | SynExpr.App(isInfix = false; funcExpr = IdentName "not"; argExpr = inner) ->
        match stripParens inner with
        | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ x; prop ])) when prop.idText = "IsSome" ->
            ValueSome(x, true)
        | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ x; prop ])) when prop.idText = "IsNone" ->
            ValueSome(x, false)
        | _ -> ValueNone
    | _ -> ValueNone

/// Find IsSome/Value conditionals. Requires typed check results.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        // is `name` rebound anywhere inside `r` (lambda parameter, let,
        // match clause, loop pattern)? substituting under a shadow would
        // change which value the binder refers to
        let shadowedIn (name: string) (r: range) =
            index.Exprs
            |> Array.exists (fun (_, e) ->
                Range.rangeContainsRange r e.Range
                && (let boundPats =
                        match e with
                        | SynExpr.Lambda(parsedData = Some(pats, _)) -> pats
                        | SynExpr.LetOrUse lou -> lou.Bindings |> List.map (fun (SynBinding(headPat = p)) -> p)
                        | SynExpr.Match(clauses = clauses)
                        | SynExpr.MatchBang(clauses = clauses)
                        | SynExpr.MatchLambda(matchClauses = clauses) ->
                            clauses |> List.map (fun (SynMatchClause(pat = p)) -> p)
                        | SynExpr.ForEach(pat = p) -> [ p ]
                        | _ -> []

                    boundPats |> List.exists (fun p -> patBoundNames p |> List.contains name)))

        // `x.Value` prefixes inside `r`: the sub-range covering `x.Value`
        let valueUses (x: string) (r: range) =
            index.Exprs
            |> Array.choose (fun (_, e) ->
                match e with
                | SynExpr.LongIdent(longDotId = SynLongIdent(id = first :: second :: _)) when
                    first.idText = x
                    && second.idText = "Value"
                    && Range.rangeContainsRange r e.Range
                    ->
                    Some(Range.mkRange e.Range.FileName e.Range.Start second.idRange.End)
                | _ -> None)

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.IfThenElse(ifExpr = OptionTest(x, negated); thenExpr = t; elseExpr = els; trivia = trivia) when
                  not trivia.IsElif && isSingleLine expr.Range
                  ->
                  let someArm, noneArm = if negated then els, Some t else Some t, els

                  match someArm with
                  | Some someExpr when
                      (valueUses x.idText someExpr.Range).Length > 0
                      && not (shadowedIn x.idText someExpr.Range)
                      && (noneArm |> Option.forall (fun n -> (valueUses x.idText n.Range).Length = 0))
                      ->
                      match caseNamesFor check source x with
                      | Some(someCase, noneCase) ->
                          let wholeText = textOfRange source expr.Range

                          let binder =
                              [ "v"; $"{x.idText}Value" ]
                              |> List.tryFind (fun name ->
                                  not (Regex.IsMatch(wholeText, @"\b" + Regex.Escape name + @"\b")))

                          match binder with
                          | Some binder ->
                              // substitute x.Value prefixes right-to-left
                              let substituted (branch: SynExpr) =
                                  valueUses x.idText branch.Range
                                  |> Array.sortByDescending (fun r -> r.StartColumn)
                                  |> Array.fold
                                      (fun (text: string) (r: range) ->
                                          let start = r.StartColumn - branch.Range.StartColumn
                                          let length = r.EndColumn - r.StartColumn
                                          text.Remove(start, length).Insert(start, binder))
                                      (textOfRange source branch.Range)

                              let noneText =
                                  noneArm
                                  |> Option.map (fun n -> textOfRange source n.Range)
                                  |> Option.defaultValue "()"

                              let replacement =
                                  sprintf
                                      "match %s with | %s %s -> %s | %s -> %s"
                                      x.idText
                                      someCase
                                      binder
                                      (substituted someExpr)
                                      noneCase
                                      noneText

                              { Range = expr.Range
                                OriginalText = wholeText
                                ReplacementText = replacement }
                          | None -> ()
                      | None -> ()
                  | _ -> ()
              | _ -> () ]
