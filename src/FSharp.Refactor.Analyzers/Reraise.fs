/// Refactoring (correctness, CA2200): re-raising a caught exception with
/// `raise` resets its stack trace; `reraise ()` preserves it.
///
///     try ... with ex ->        try ... with ex ->
///         log ex           →        log ex
///         raise ex                  reraise ()
///
/// Losing the original stack trace is the classic way production bugs
/// become undiagnosable, so this fix intentionally changes the observable
/// stack trace — back to the one the code meant to keep.
///
/// Safety rules:
///   - the raised identifier is bound by the handler's own pattern and is
///     not rebound in between
///   - the raise site is lexically in the handler: not inside a lambda,
///     computation expression, or nested try (where `reraise` would not
///     compile or would refer to a different exception)
///   - the try-with itself is not inside a computation expression:
///     `task { try ... with ex -> raise ex }` desugars the handler into a
///     lambda passed to builder.TryWith, where `reraise ()` is FS0413
///   - `raise` resolves (typed check results) to FSharp.Core
module FSharp.Refactor.Reraise

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        Range: range
        OriginalText: string
        /// The exception identifier, for the message.
        ExceptionName: string
    }

/// Find `raise ex` sites in with-handlers. Requires typed check results.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        // sub-ranges of `r` where reraise () would not compile or would
        // mean a different exception
        let opaqueRangesIn (r: range) =
            index.Exprs
            |> Array.choose (fun (_, e) ->
                match e with
                | SynExpr.Lambda _
                | SynExpr.MatchLambda _
                | SynExpr.ComputationExpr _
                | SynExpr.TryWith _
                | SynExpr.TryFinally _ when Range.rangeContainsRange r e.Range -> Some e.Range
                | _ -> None)

        // is `name` rebound inside `r`?
        let reboundIn (name: string) (r: range) =
            index.Exprs
            |> Array.exists (fun (_, e) ->
                Range.rangeContainsRange r e.Range
                && (match e with
                    | SynExpr.LetOrUse lou ->
                        lou.Bindings
                        |> List.exists (fun (SynBinding(headPat = p)) -> patBoundNames p |> List.contains name)
                    | SynExpr.Lambda(parsedData = Some(pats, _)) ->
                        pats |> List.exists (fun p -> patBoundNames p |> List.contains name)
                    | _ -> false))

        // A try-with whose nearest deferring ancestor is a computation
        // expression desugars into builder.TryWith(body, handler) — the
        // handler becomes a lambda, where reraise () is FS0413. A lambda or
        // object-expression member between the two resets to ordinary code.
        let inComputationExpr (path: SyntaxNode list) =
            path
            |> List.tryPick (fun node ->
                match node with
                | SyntaxNode.SynExpr(SynExpr.ComputationExpr _)
                | SyntaxNode.SynExpr(SynExpr.ArrayOrListComputed _) -> Some true
                | SyntaxNode.SynExpr(SynExpr.Lambda _)
                | SyntaxNode.SynExpr(SynExpr.MatchLambda _)
                | SyntaxNode.SynExpr(SynExpr.ObjExpr _) -> Some false
                | _ -> None)
            |> Option.defaultValue false

        [ for path, expr in index.Exprs do
              match expr with
              | SynExpr.TryWith(withCases = clauses) when not (inComputationExpr path) ->
                  for SynMatchClause(pat = pat; resultExpr = handler) in clauses do
                      let exNames = patBoundNames pat |> Set.ofList

                      if not exNames.IsEmpty then
                          let opaque = opaqueRangesIn handler.Range

                          for _, e in index.Exprs do
                              match e with
                              | SynExpr.App(isInfix = false; funcExpr = SingleIdent raiseId; argExpr = arg) when
                                  raiseId.idText = "raise"
                                  && Range.rangeContainsRange handler.Range e.Range
                                  && not (opaque |> Array.exists (fun o -> Range.rangeContainsRange o e.Range))
                                  ->
                                  match stripParens arg with
                                  | SynExpr.Ident exId when
                                      exNames.Contains exId.idText
                                      && not (reboundIn exId.idText handler.Range)
                                      && OptionModule.resolvesToCoreOperator check source raiseId
                                      ->
                                      { Range = e.Range
                                        OriginalText = textOfRange source e.Range
                                        ExceptionName = exId.idText }
                                  | _ -> ()
                              | _ -> ()
              | _ -> () ]
