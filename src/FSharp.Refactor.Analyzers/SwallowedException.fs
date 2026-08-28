/// Refactoring note (correctness): a catch-all handler that does nothing —
/// or quietly substitutes a default value — swallows every exception.
///
///     try work () with _ -> ()              // hides bugs AND cancellation
///     try work () with :? Exception -> ()
///     try read () with _ -> ""              // masks failure as an answer
///     try count () with _ -> 0
///     try get () with _ -> Unchecked.defaultof<_>
///
/// An empty catch of System.Exception silently eats programming errors,
/// OperationCanceledException, and everything else; a default-value catch
/// additionally disguises the failure as a legitimate result. Advice: log
/// or `reraise ()` — and catch the specific exception type the code can
/// actually handle instead of Exception.
///
/// Only trivially empty or constant-default bodies with catch-all patterns
/// (`_`, a bare binder, or `:? System.Exception`) are flagged; a handler
/// that catches a SPECIFIC exception type and deliberately ignores it is a
/// decision, not an accident, and stays quiet.
module FSharp.Refactor.SwallowedException

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        Range: range
        /// The handler pattern's text, for the message.
        PatternText: string
        /// The substituted default's text (`""`, `0`, `Unchecked.defaultof`),
        /// or None for an empty `()` body.
        FallbackText: string option
    }

/// A pattern that matches every exception.
let private isCatchAll (pat: SynPat) =
    match pat with
    | SynPat.Wild _
    | SynPat.Named _ -> true
    | SynPat.IsInst(SynType.LongIdent(SynLongIdent(id = ids)), _) ->
        not ids.IsEmpty && (List.last ids).idText = "Exception"
    | SynPat.As(lhsPat = SynPat.IsInst(SynType.LongIdent(SynLongIdent(id = ids)), _)) ->
        not ids.IsEmpty && (List.last ids).idText = "Exception"
    | _ -> false

/// A body that substitutes a default-ish value for the exception: a bare
/// constant, `Unchecked.defaultof<_>`, None/ValueNone, or an empty
/// collection literal.
let private isDefaultFallback (e: SynExpr) =
    match e with
    | SynExpr.Const(SynConst.Unit, _) -> false // handled as the empty body
    // `try ping (); true with _ -> false` is the idiomatic bool probe —
    // the failure IS the answer there, so bools stay quiet
    | SynExpr.Const(SynConst.Bool _, _) -> false
    | SynExpr.Const _ -> true
    | SynExpr.Null _ -> true
    | IdentName("None" | "ValueNone") -> true
    | SynExpr.ArrayOrList(_, [], _) -> true
    // dotted defaults: String.Empty, DateTime.MinValue, TimeSpan.Zero,
    // Array.empty, Map.empty, ...
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
        not ids.IsEmpty
        && (match (List.last ids).idText with
            | "Empty"
            | "empty"
            | "MinValue"
            | "Zero"
            | "Default" -> true
            | _ -> false)
        ->
        true
    | SynExpr.TypeApp(expr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))) ->
        not ids.IsEmpty && (List.last ids).idText = "defaultof"
    | _ -> false

[<return: Struct>]
let inline private (|IsDefaultFallback|_|) input =
    if isDefaultFallback input then
        ValueSome input
    else
        ValueNone

/// Find empty and default-substituting catch-all handlers.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    [ for _, expr in index.Exprs do
          match expr with
          | SynExpr.TryWith(withCases = clauses) ->
              for clause in clauses do
                  match simpleClause clause with
                  | Some(pat, result) when isCatchAll pat ->
                      match stripParens result with
                      | UnitConst ->
                          { Range = clause.Range
                            PatternText = textOfRange source pat.Range
                            FallbackText = None }
                      | IsDefaultFallback body ->
                          { Range = clause.Range
                            PatternText = textOfRange source pat.Range
                            FallbackText = Some(textOfRange source body.Range) }
                      | _ -> ()
                  | _ -> ()
          | _ -> () ]
