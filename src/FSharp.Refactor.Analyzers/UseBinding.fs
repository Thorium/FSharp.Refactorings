/// Refactoring (correctness): a locally constructed disposable bound with
/// `let` has no owner — nothing ever disposes it.
///
///     let stream = new FileStream(path, FileMode.Open)     // leaked
///     use stream = new FileStream(path, FileMode.Open)     // disposed at
///                                                          // scope exit
///
/// The sibling of FR0032 (disposable FIELDS without IDisposable), for
/// expression-level bindings.
///
/// Two tiers:
///   - FIX (`let` → `use`) when the value provably stays inside the scope:
///     every mention is a member access (`x.Read ...`), never a bare
///     identifier (bare = passed, stored, or returned — ownership may
///     transfer), never inside a lambda (which may outlive the scope), and
///     no result position mentions it at all
///   - NOTE ONLY when mentions exist that could move the value elsewhere:
///     the leak is still worth pointing out, the rewrite is the author's
///     call
///
/// Skips entirely when the scope already calls `x.Dispose()` — that is
/// manual management, not a leak.
module FSharp.Refactor.UseBinding

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        /// The `let` keyword's range (the fix rewrites it to `use`).
        Range: range
        Name: string
        /// None = advisory only (the value may escape the scope).
        Fix: (string * string) option
    }

/// The result-position expressions of a body: what the scope evaluates to.
[<TailCall>]
let rec private resultsLoop (acc: SynExpr list) (pending: SynExpr list) =
    match pending with
    | [] -> acc
    | e :: rest ->
        match e with
        | SynExpr.Sequential(expr2 = e2) -> resultsLoop acc (e2 :: rest)
        | SynExpr.LetOrUse lou -> resultsLoop acc (lou.Body :: rest)
        | SynExpr.IfThenElse(thenExpr = t; elseExpr = els) ->
            let next =
                els
                |> Option.map (fun e2 -> t :: e2 :: rest)
                |> Option.defaultWith (fun () -> t :: rest)

            resultsLoop acc next
        | SynExpr.Match(clauses = clauses)
        | SynExpr.MatchBang(clauses = clauses) ->
            resultsLoop acc ((clauses |> List.map (fun (SynMatchClause(resultExpr = r)) -> r)) @ rest)
        | SynExpr.TryWith(tryExpr = t; withCases = clauses) ->
            resultsLoop acc (t :: (clauses |> List.map (fun (SynMatchClause(resultExpr = r)) -> r)) @ rest)
        | SynExpr.TryFinally(tryExpr = t) -> resultsLoop acc (t :: rest)
        | SynExpr.Paren(expr = inner)
        | SynExpr.Typed(expr = inner) -> resultsLoop acc (inner :: rest)
        | SynExpr.While _
        | SynExpr.For _
        | SynExpr.ForEach _ -> resultsLoop acc rest
        | other -> resultsLoop (other :: acc) rest

/// Find leaked local disposables. Requires typed check results for the
/// IDisposable gate.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.LetOrUse lou when not (lou.IsBang || lou.IsUse || lou.IsRecursive) ->
                  match lou.Bindings with
                  | [ SynBinding(
                          isMutable = false
                          headPat = SynPat.Named(ident = SynIdent(ident = binder); accessibility = None)
                          expr = SynExpr.New _) ] when ObjectDesign.resolvesToDisposable check source binder ->
                      let name = binder.idText
                      let body = lou.Body

                      // classify every mention of the binder in the scope
                      let mentions =
                          index.Exprs
                          |> Array.filter (fun (_, e) ->
                              match e with
                              | SynExpr.Ident id when id.idText = name ->
                                  Range.rangeContainsRange body.Range id.idRange
                              | SynExpr.LongIdent(longDotId = SynLongIdent(id = firstId :: _)) when
                                  firstId.idText = name
                                  ->
                                  Range.rangeContainsRange body.Range firstId.idRange
                              | _ -> false)

                      let manuallyDisposed =
                          mentions
                          |> Array.exists (fun (_, e) ->
                              match e with
                              | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ _; m ])) -> m.idText = "Dispose"
                              | _ -> false)

                      if not manuallyDisposed then
                          let bareUses =
                              mentions
                              |> Array.exists (fun (_, e) ->
                                  match e with
                                  | SynExpr.Ident _ -> true
                                  | _ -> false)

                          let inLambda =
                              mentions
                              |> Array.exists (fun (path, _) ->
                                  path
                                  |> List.exists (fun node ->
                                      match node with
                                      | SyntaxNode.SynExpr(SynExpr.Lambda _) ->
                                          // only lambdas INSIDE this scope count
                                          true
                                      | _ -> false))

                          let results = resultsLoop [] [ body ]

                          let inResult =
                              mentions
                              |> Array.exists (fun (_, e) ->
                                  results |> List.exists (fun r -> Range.rangeContainsRange r.Range e.Range))

                          // the `let` keyword: the LetOrUse node starts at it
                          let letRange =
                              Range.mkRange
                                  expr.Range.FileName
                                  expr.Range.Start
                                  (Position.mkPos expr.Range.StartLine (expr.Range.StartColumn + 3))

                          if textOfRange source letRange = "let" then
                              let canFix = not (bareUses || inLambda || inResult)

                              { Range = letRange
                                Name = name
                                Fix = if canFix then Some("let", "use") else None }
                  | _ -> ()
              | _ -> () ]
