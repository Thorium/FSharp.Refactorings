/// Refactoring: extract a function composition from a lambda.
///
///     xs |> List.map (fun x -> x |> f |> g)      →  xs |> List.map (f >> g)
///     xs |> List.map (fun x -> g (f x))          →  xs |> List.map (f >> g)
///     xs |> List.map (fun x -> x |> List.map f |> List.filter g)
///                                                →  xs |> List.map (List.map f >> List.filter g)
///
/// Safety rules:
///   - only lambdas that are directly parenthesized, i.e. used as arguments;
///     a `let h = fun x -> ...` binding is left alone (rewriting it to
///     `let h = f >> g` changes generalization under the value restriction)
///   - single unannotated parameter, at least two composed stages
///   - no stage may mention the parameter (checked conservatively on the
///     stage's source text, so shadowing tricks never produce a wrong rewrite)
///   - stages must be single-line; stages that are not plain applications are
///     parenthesized in the output
///
/// Known caveat: a stage that is a partial application (`x |> h y`) is
/// evaluated per invocation in the lambda but once at construction in the
/// composition; for the overwhelmingly common pure partial applications
/// (`List.map f`) this is unobservable, but a function that runs effects
/// before returning its closure would run them fewer times.
module FSharp.Refactorings.Composition

open System.Text.RegularExpressions
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

type Suggestion =
    {
        /// Range of the whole lambda, i.e. the text the fix replaces.
        Range: range
        OriginalText: string
        ReplacementText: string
    }

/// A stage that can be written bare between `>>`s: an identifier or a plain
/// (non-infix) curried application of one, e.g. `f`, `List.map`, `List.map f`.
[<TailCall>]
let rec private isPlainApplication (e: SynExpr) =
    match e with
    | SynExpr.Ident _
    | SynExpr.LongIdent _
    | SynExpr.Const _
    | SynExpr.Paren _
    | SynExpr.DotGet _ -> true
    | SynExpr.App(isInfix = true) -> false
    | SynExpr.TypeApp(expr = inner) -> isPlainApplication inner
    | SynExpr.App(funcExpr = funcExpr) -> isPlainApplication funcExpr
    | _ -> false

[<TailCall>]
let rec private pipeStagesLoop (param: string) (e: SynExpr) (acc: SynExpr list) =
    match e with
    | PipeApp(lhs, rhs) -> pipeStagesLoop param lhs (rhs :: acc)
    | SynExpr.Ident ident when ident.idText = param -> Some acc
    | _ -> None

/// `fun x -> x |> stage1 |> stage2 |> ...` — stages in composition order.
let private pipeStages (param: string) (e: SynExpr) : SynExpr list option = pipeStagesLoop param e []

/// `fun x -> g (f x)` — functions in composition order (innermost first).
[<TailCall>]
let rec private applicationStagesLoop (param: string) (e: SynExpr) (acc: SynExpr list) =
    match stripParens e with
    | SynExpr.Ident ident when ident.idText = param -> Some acc
    // the funcExpr must not itself be an infix application: `1 + x` parses as
    // App(App(op, 1), x), and peeling x off would leave the invalid stage `1 +`
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.App(isInfix = true)) -> None
    | SynExpr.App(isInfix = false; funcExpr = funcExpr; argExpr = argExpr) ->
        applicationStagesLoop param argExpr (funcExpr :: acc)
    | _ -> None

/// `fun x -> g (f x)` — functions in composition order (innermost first).
let private applicationStages (param: string) (body: SynExpr) : SynExpr list option =
    applicationStagesLoop param body []

let private stageText (source: ISourceText) (stage: SynExpr) =
    let text = textOfRange source stage.Range
    if isPlainApplication stage then text else $"({text})"

/// Conservative free-variable check: reject the rewrite if the parameter name
/// appears anywhere in the stage's text (over-approximates, which is the safe
/// direction).
let private mentionsParam (param: string) (text: string) =
    Regex.IsMatch(text, sprintf @"\b%s\b" (Regex.Escape param))

/// Find all parenthesized lambdas that are pipelines or nested applications of
/// their parameter and can be rewritten as `f >> g` compositions.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(path, expr) =
                match expr with
                | SynExpr.Lambda(parsedData = Some([ SynPat.Named(ident = SynIdent(ident = param)) ], body)) ->
                    // genuine argument position only: a parenthesized lambda
                    // bound by a let (`let h = (fun x -> ...)`) is generalized,
                    // while the composition it would become is an application
                    // and falls under the value restriction
                    let isArgument =
                        match path with
                        | SyntaxNode.SynExpr(SynExpr.Paren _) :: SyntaxNode.SynExpr(SynExpr.App _) :: _ -> true
                        | _ -> false

                    if isArgument then
                        let stages =
                            match pipeStages param.idText body with
                            | Some stages -> Some stages
                            | None -> applicationStages param.idText body

                        match stages with
                        | Some stages when
                            List.length stages >= 2
                            && stages |> List.forall (fun s -> isSingleLine s.Range)
                            && stages
                               |> List.forall (fun s -> not (mentionsParam param.idText (textOfRange source s.Range)))
                            ->
                            let replacement = stages |> List.map (stageText source) |> String.concat " >> "

                            suggestions.Add
                                { Range = expr.Range
                                  OriginalText = textOfRange source expr.Range
                                  ReplacementText = replacement }
                        | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq suggestions
