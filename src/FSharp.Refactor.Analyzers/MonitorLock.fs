/// FR0123 (fix): the canonical Monitor.Enter/try/finally/Monitor.Exit
/// shape IS F#'s `lock` function — which releases on all paths by
/// construction, closing the whole released-on-every-path rule family
/// at the source.
///
///     Monitor.Enter gate                lock gate (fun () ->
///     try                                   body
///         body                          )
///     finally
///         Monitor.Exit gate
///
/// The body under `try` already sits at exactly the indentation the
/// lambda needs, so it moves VERBATIM — comments included.
///
/// Gates: single-argument Enter (the `(x, &taken)` overload carries
/// protocol this rewrite would erase), the SAME lock expression text in
/// Enter and Exit, the finally holding nothing but the Exit, own-line
/// statements, and the Monitor entity typed-verified. A bare
/// Monitor.Enter with no try/finally at all is the note: the lock leaks
/// on the first exception.
module FSharp.Refactor.MonitorLock

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        Range: range
        /// Present for the canonical shape: whole-region replacement.
        Fix: (range * string * string) option
        LockText: string
    }

/// `Monitor.<method> arg` with the single argument expression.
[<return: Struct>]
let private (|MonitorCall|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = arg) when
        ids.Length >= 2 && (ids |> List.item (ids.Length - 2)).idText = "Monitor"
        ->
        match stripParens arg with
        | SynExpr.Tuple _ -> ValueNone // Enter(x, &taken) carries protocol
        | single -> ValueSome((List.last ids), single)
    | _ -> ValueNone

let private isMonitorEntity (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as mfv ->
            (try
                mfv.DeclaringEntity
                |> Option.bind (fun e -> e.TryFullName)
                |> Option.map ((=) "System.Threading.Monitor")
                |> Option.defaultValue false
             with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                 false)
        | _ -> false
    | None -> false

let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        // the rewrite wraps the body in a LAMBDA: computation binds
        // (do!/let!/yield) stop compiling there, and a closure cannot
        // capture a local mutable declared outside itself
        let bindLikeRanges =
            index.Exprs
            |> Array.choose (fun (_, e) ->
                match e with
                | SynExpr.LetOrUse lou when lou.IsBang -> Some e.Range
                | SynExpr.DoBang _
                | SynExpr.YieldOrReturn _
                | SynExpr.YieldOrReturnFrom _
                | SynExpr.MatchBang _ -> Some e.Range
                | _ -> None)

        let containsBindLike (r: range) =
            bindLikeRanges |> Array.exists (fun b -> Range.rangeContainsRange r b)

        let localMutables =
            index.Exprs
            |> Array.collect (fun (_, e) ->
                match e with
                | SynExpr.LetOrUse lou when not lou.IsBang ->
                    lou.Bindings
                    |> List.choose (fun b ->
                        match b with
                        | SynBinding(isMutable = true; headPat = SynPat.Named(ident = SynIdent(ident = id))) ->
                            Some(id.idText, b.RangeOfBindingWithRhs)
                        | _ -> None)
                    |> Array.ofList
                | _ -> [||])

        let mentionsForeignMutable (blockRange: range) (text: string) =
            localMutables
            |> Array.exists (fun (name, declRange) ->
                not (Range.rangeContainsRange blockRange declRange)
                && System.Text.RegularExpressions.Regex.IsMatch(text, identifierPattern name))

        let startsOwnLine (r: range) =
            r.StartColumn = 0
            || (source.GetLineString(r.StartLine - 1)).Substring(0, r.StartColumn).Trim() = ""

        let lineTailBlank (r: range) =
            (source.GetLineString(r.EndLine - 1)).Substring(r.EndColumn).Trim() = ""

        // Enter statements followed by a guarding try/finally, per the
        // canonical Sequential(Enter, TryFinally(body, Exit)) chain
        let guarded = System.Collections.Generic.HashSet<int * int>()

        let canonical =
            [ for _, e in index.Exprs do
                  match e with
                  | SynExpr.Sequential(
                      expr1 = MonitorCall(enterId, lockArg) & enterExpr
                      expr2 = SynExpr.TryFinally(
                          tryExpr = body; finallyExpr = MonitorCall(exitId, exitArg); trivia = tfTrivia) & tf) when
                      enterId.idText = "Enter" && exitId.idText = "Exit"
                      ->
                      guarded.Add(enterExpr.Range.StartLine, enterExpr.Range.StartColumn)
                      |> ignore

                      let lockText = textOfRange source lockArg.Range
                      let tryLine = tfTrivia.TryKeyword.StartLine
                      let finallyLine = tfTrivia.FinallyKeyword.StartLine

                      let fix =
                          if
                              lockText = textOfRange source exitArg.Range
                              && isMonitorEntity check source enterId
                              && startsOwnLine enterExpr.Range
                              && startsOwnLine tfTrivia.TryKeyword
                              && startsOwnLine tfTrivia.FinallyKeyword
                              // body strictly between the keyword lines, so
                              // every line — comments included — travels
                              && body.Range.StartLine > tryLine
                              && body.Range.EndLine < finallyLine
                              && lineTailBlank body.Range
                              && not (containsBindLike body.Range)
                              && not (spansDirective source e.Range)
                          then
                              let indent = String.replicate enterExpr.Range.StartColumn " "

                              let bodyLines =
                                  [ for l in tryLine + 1 .. finallyLine - 1 -> source.GetLineString(l - 1) ]
                                  |> String.concat "\n"

                              let replaceRange =
                                  Range.mkRange e.Range.FileName enterExpr.Range.Start tf.Range.End

                              let bodyRegion =
                                  Range.mkRange
                                      e.Range.FileName
                                      (Position.mkPos (tryLine + 1) 0)
                                      (Position.mkPos finallyLine 0)

                              if mentionsForeignMutable bodyRegion bodyLines then
                                  // the lambda could not capture it (FS0407)
                                  None
                              else
                                  Some(
                                      replaceRange,
                                      textOfRange source replaceRange,
                                      $"lock {lockText} (fun () ->\n{bodyLines.TrimEnd()}\n{indent})"
                                  )
                          else
                              None

                      yield
                          { Range = enterExpr.Range
                            Fix = fix
                            LockText = lockText }
                  | _ -> () ]

        // bare Enter with no guarding try at all: leaks on first exception
        let bare =
            [ for _, e in index.Exprs do
                  match e with
                  | MonitorCall(enterId, lockArg) when
                      enterId.idText = "Enter"
                      && not (guarded.Contains(e.Range.StartLine, e.Range.StartColumn))
                      && isMonitorEntity check source enterId
                      ->
                      { Range = e.Range
                        Fix = None
                        LockText = textOfRange source lockArg.Range }
                  | _ -> () ]

        canonical @ bare
