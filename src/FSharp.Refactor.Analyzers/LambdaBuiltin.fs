/// Refactoring: a lambda that only reproduces a built-in function.
///
///     fun x -> x           →  id
///     fun (a, b) -> a      →  fst
///     fun (a, b) -> b      →  snd
///
/// Safety rules:
///   - one parameter only. `fun x y -> x` is not `fst`: it takes its
///     arguments curried, where `fst` takes one tuple.
///   - the pattern must be plain names, unannotated. `fun (a: int, b) -> a`
///     carries a type annotation that the call site may be relying on, and
///     `fst` cannot carry it.
///   - not as a direct argument to a .NET method. `xs.Select(fun x -> x)`
///     relies on F# converting a lambda EXPRESSION to a delegate; a function
///     value does not convert the same way, so `xs.Select(id)` need not
///     compile. Arguments to F# functions (`List.map id`) are unaffected.
module FSharp.Refactor.LambdaBuiltin

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactor.Text

type Suggestion =
    {
        /// Range of the whole lambda, i.e. the text the fix replaces.
        Range: range
        OriginalText: string
        ReplacementText: string
    }

/// A pattern that is exactly one unannotated name, yielding that name.
[<return: Struct>]
let private (|PlainName|_|) (pat: SynPat) =
    match pat with
    | SynPat.Named(ident = SynIdent(ident = id)) -> ValueSome id.idText
    | _ -> ValueNone

/// `(a, b)` — a two-element tuple of plain names, in either paren style.
[<return: Struct>]
let private (|PlainNamePair|_|) (pat: SynPat) =
    let unwrapped =
        match pat with
        | SynPat.Paren(pat = inner) -> inner
        | other -> other

    match unwrapped with
    | SynPat.Tuple(elementPats = [ PlainName first; PlainName second ]) -> ValueSome(first, second)
    | _ -> ValueNone

/// The identifier an expression is, if it is nothing more than one.
[<return: Struct>]
let private (|JustIdent|_|) (e: SynExpr) =
    match e with
    | SynExpr.Ident id -> ValueSome id.idText
    | _ -> ValueNone

/// The callee of the application this node sits in, seen through the call's
/// own parentheses: `xs.Select(fun x -> x)` wraps the lambda in a `Paren`
/// before the `App` that decides anything.
[<TailCall>]
let rec private enclosingCallee (path: SyntaxNode list) =
    match path with
    | SyntaxNode.SynExpr(SynExpr.Paren _) :: rest -> enclosingCallee rest
    | SyntaxNode.SynExpr(SynExpr.App(funcExpr = funcExpr)) :: _ -> Some funcExpr
    | _ -> None

/// Does this callee name a .NET method rather than an F# function?
let private isMethod (funcExpr: SynExpr) =
    match funcExpr with
    | SynExpr.DotGet _ -> true
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty ->
        let last = (List.last ids).idText
        last.Length > 0 && Char.IsUpper last.[0]
    | SynExpr.Ident id -> id.idText.Length > 0 && Char.IsUpper id.idText.[0]
    | _ -> false

/// Is this lambda a direct argument to a .NET method call, where the
/// lambda-to-delegate conversion may be doing the work?
let private argumentToMethod (path: SyntaxNode list) =
    enclosingCallee path |> Option.exists isMethod

/// Find lambdas that are just `id`, `fst` or `snd`.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(path, expr) =
                match expr with
                // `parsedData` carries the patterns as written, before the
                // compiler lowers them into SynSimplePats
                | SynExpr.Lambda(parsedData = Some([ parameter ], body)) when not (argumentToMethod path) ->
                    let builtin =
                        match parameter, body with
                        | PlainName only, JustIdent returned when only = returned -> Some "id"
                        | PlainNamePair(first, second), JustIdent returned when returned = first -> Some "fst"
                        | PlainNamePair(first, second), JustIdent returned when returned = second -> Some "snd"
                        | _ -> None

                    match builtin with
                    | Some replacement ->
                        suggestions.Add
                            { Range = expr.Range
                              OriginalText = textOfRange source expr.Range
                              ReplacementText = replacement }
                    | None -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq suggestions
