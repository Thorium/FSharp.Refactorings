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
    // bools are decided by the caller, which can see the try BODY: the
    // `try ping (); true with _ -> false` probe stays quiet, while
    // `try parse s with _ -> false` disguises the failure as an answer
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

/// The expression a block evaluates to — the tail of its Sequential chain.
[<TailCall>]
let rec private lastExprOf (e: SynExpr) =
    match e with
    | SynExpr.Sequential(expr2 = e2) -> lastExprOf e2
    | SynExpr.Paren(expr = inner) -> lastExprOf inner
    | _ -> e

/// Is a bool-literal catch-all the PROBE idiom — the try body answering
/// with the opposite literal (`try ping (); true with _ -> false`, or the
/// inverted did-it-throw probe)? Then the failure IS the answer. Any other
/// body makes the literal a disguised default like the rest.
let private isBoolProbe (tryBody: SynExpr) (fallback: bool) =
    match lastExprOf tryBody with
    | SynExpr.Const(SynConst.Bool bodyValue, _) -> bodyValue <> fallback
    | _ -> false

/// Find empty and default-substituting catch-all handlers.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    [ for _, expr in index.Exprs do
          match expr with
          | SynExpr.TryWith(tryExpr = tryBody; withCases = clauses) ->
              for clause in clauses do
                  match simpleClause clause with
                  | Some(pat, result) when isCatchAll pat ->
                      match stripParens result with
                      | UnitConst ->
                          { Range = clause.Range
                            PatternText = textOfRange source pat.Range
                            FallbackText = None }
                      | SynExpr.Const(SynConst.Bool fallback, _) as body when not (isBoolProbe tryBody fallback) ->
                          { Range = clause.Range
                            PatternText = textOfRange source pat.Range
                            FallbackText = Some(textOfRange source body.Range) }
                      | IsDefaultFallback body ->
                          { Range = clause.Range
                            PatternText = textOfRange source pat.Range
                            FallbackText = Some(textOfRange source body.Range) }
                      | _ -> ()
                  | _ -> ()
          | _ -> () ]
