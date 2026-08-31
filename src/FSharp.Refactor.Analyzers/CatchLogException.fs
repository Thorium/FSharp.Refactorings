/// FR0120 (fix): a log call inside an exception handler that never
/// mentions the caught exception — the one fact the handler exists to
/// record.
///
///     with ex ->                          with ex ->
///         logger.LogError("sync failed {Id}", id)
///                          →   logger.LogError(ex, "sync failed {Id}", id)
///
/// The primary fix passes the exception itself (ILogger's
/// exception-first overload; the SINK decides rendering). The editor
/// also offers `ex.GetBaseException()` — the root cause of a wrapped or
/// aggregate exception.
///
/// ANY existing mention of the exception in the call's arguments counts
/// as handled — `ex.Message` included: logging only the message is a
/// legitimate GDPR/PII choice, and this rule must not escalate it to a
/// full stack trace.
///
/// Typed-gated to Microsoft.Extensions.Logging's extension methods, so
/// a user type with a `LogError` member never matches.
module FSharp.Refactor.CatchLogException

open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        /// Zero-width, at the first argument — `ex, ` goes here.
        Range: range
        ExceptionName: string
        LogMethod: string
    }

let private logMethods = set [ "LogError"; "LogCritical"; "LogWarning" ]

/// The name a handler clause binds its exception to.
[<TailCall>]
let rec private exceptionNameOf (p: SynPat) =
    match p with
    | SynPat.Named(ident = SynIdent(ident = id)) -> ValueSome id.idText
    | SynPat.As(rhsPat = SynPat.Named(ident = SynIdent(ident = id))) -> ValueSome id.idText
    | SynPat.Paren(pat = inner) -> exceptionNameOf inner
    | _ -> ValueNone

[<return: Struct>]
let private (|CallIdent|_|) (e: SynExpr) =
    match e with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> ValueSome(List.last ids)
    | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> ValueSome(List.last ids)
    | _ -> ValueNone

let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        // handler clauses that bind an exception, with their body ranges
        let handlers =
            [ for _, e in index.Exprs do
                  match e with
                  | SynExpr.TryWith(withCases = cases) ->
                      for SynMatchClause(pat = p; resultExpr = result) in cases do
                          match exceptionNameOf p with
                          | ValueSome name -> yield name, result.Range
                          | ValueNone -> ()
                  | _ -> () ]

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.App(isInfix = false; funcExpr = CallIdent logId; argExpr = SynExpr.Paren(expr = inner)) when
                  logMethods.Contains logId.idText
                  // the exception-first overload only pairs with a template
                  // in FIRST position — `LogError(eventId, "msg")` wants the
                  // exception SECOND, and inserting it first matches nothing
                  && (match inner with
                      | SynExpr.Const(SynConst.String _, _)
                      | SynExpr.InterpolatedString _ -> true
                      | SynExpr.Tuple(exprs = first :: _) ->
                          (match first with
                           | SynExpr.Const(SynConst.String _, _)
                           | SynExpr.InterpolatedString _ -> true
                           | _ -> false)
                      | _ -> false)
                  ->
                  // the innermost handler this call sits in
                  let handler =
                      handlers
                      |> List.filter (fun (_, body) -> Range.rangeContainsRange body expr.Range)
                      |> List.sortBy (fun (_, body) -> body.EndLine - body.StartLine, body.EndColumn)
                      |> List.tryHead

                  match handler with
                  | Some(exName, _) when
                      // any mention of the exception — ex, ex.Message,
                      // ex.ToString() — means the author already chose what
                      // to record
                      not (Regex.IsMatch(textOfRange source inner.Range, identifierPattern exName))
                      ->
                      // typed gate: Microsoft.Extensions.Logging extensions
                      let isLoggerExtension =
                          let r = logId.idRange
                          let lineText = source.GetLineString(r.EndLine - 1)

                          match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ logId.idText ]) with
                          | Some symbolUse ->
                              match symbolUse.Symbol with
                              | :? FSharpMemberOrFunctionOrValue as mfv ->
                                  (try
                                      mfv.DeclaringEntity
                                      |> Option.bind (fun e -> e.TryFullName)
                                      |> Option.map (fun n -> n.StartsWith "Microsoft.Extensions.Logging")
                                      |> Option.defaultValue false
                                   with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                                       false)
                              | _ -> false
                          | None -> false

                      if isLoggerExtension then
                          { Range = Range.mkRange expr.Range.FileName inner.Range.Start inner.Range.Start
                            ExceptionName = exName
                            LogMethod = logId.idText }
                  | _ -> ()
              | _ -> () ]
