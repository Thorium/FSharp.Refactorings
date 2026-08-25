/// Refactoring: strip computation-expression wrapping that does nothing.
///
///     async { return! comp }                          →  comp
///     async { let! v = comp in return v }             →  comp
///     async { return e } |> Async.RunSynchronously    →  e
///     Async.RunSynchronously (async { return e })     →  e
///     task { return x }                               →  Task.FromResult(x)
///
/// Only *type-preserving* strips are offered: the first two keep the
/// `Async<'T>` type (monad right-identity), and in the runner form the
/// wrapper and the immediate run cancel out. Rewriting `async { return e }`
/// alone to `e` would change the expression's type and is deliberately out of
/// scope (that is a signature change, not a cleanup).
///
/// Safety rules:
///   - the stripped computation in the first two forms must be a bare
///     identifier — evaluating it early has no effects
///   - `use!` is never stripped (its disposal would be lost)
///   - in the runner form the returned expression must be single-line and
///     safe to inline at an arbitrary expression position
///   - the `task` form requires the returned expression to be a bare
///     identifier or constant (an expression that throws would produce a
///     faulted task in the CE but throw synchronously from Task.FromResult)
///     and the file to `open System.Threading.Tasks` so `Task.FromResult`
///     resolves; `task { return! ... }` is never touched because its
///     overloads also accept Async and stripping could change the type
module FSharp.Refactorings.CeStrip

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

[<RequireQualifiedAccess>]
type StripKind =
    /// `async { return! comp }` / `async { let! v = comp in return v }`
    | Forwarded
    /// `async { return e } |> Async.RunSynchronously`
    | WithRunner
    /// `task { return x }` → `Task.FromResult(x)`
    | TaskFromResult

type Suggestion =
    {
        /// Range of the expression the fix replaces.
        Range: range
        OriginalText: string
        ReplacementText: string
        Kind: StripKind
    }

/// A bare simple identifier — evaluating it has no side effects. Dotted
/// paths are deliberately excluded: `Service.Current` can be a static
/// property whose getter runs user code, and stripping the wrapper would
/// move that evaluation from per-run to construction time.
[<return: Struct>]
let private (|BareIdent|_|) (e: SynExpr) =
    match e with
    | SynExpr.Ident _ -> ValueSome e
    | _ -> ValueNone

/// `async { body }`
[<return: Struct>]
let private (|AsyncCe|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(funcExpr = SynExpr.Ident builder; argExpr = SynExpr.ComputationExpr(expr = body)) when
        builder.idText = "async"
        ->
        ValueSome body
    | _ -> ValueNone

/// `task { body }`
[<return: Struct>]
let private (|TaskCe|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(funcExpr = SynExpr.Ident builder; argExpr = SynExpr.ComputationExpr(expr = body)) when
        builder.idText = "task"
        ->
        ValueSome body
    | _ -> ValueNone

/// An expression whose evaluation cannot throw: a simple identifier or a
/// literal constant.
[<return: Struct>]
let private (|NonThrowing|_|) (e: SynExpr) =
    match e with
    | SynExpr.Ident _
    | SynExpr.Const _ -> ValueSome e
    | _ -> ValueNone

/// All `open` targets in the file, as dotted strings.
let private collectOpens (parseTree: ParsedInput) : Set<string> =
    let opens = ResizeArray<string>()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkSynModuleDecl(_path, decl) =
                match decl with
                | SynModuleDecl.Open(target = SynOpenDeclTarget.ModuleOrNamespace(longId = SynLongIdent(id = ids))) ->
                    opens.Add(ids |> List.map (fun i -> i.idText) |> String.concat ".")
                | _ -> () }

    AstIndex.replay collector parseTree
    Set.ofSeq opens

/// `Async.RunSynchronously`
[<return: Struct>]
let private (|RunSynchronously|_|) (e: SynExpr) =
    match e with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ])) when
        m.idText = "Async" && f.idText = "RunSynchronously"
        ->
        ValueSome()
    | _ -> ValueNone

/// The computation a do-nothing async body forwards to, if any:
/// `return! comp` or `let! v = comp in return v` with comp a bare identifier.
let private forwardedComputation (body: SynExpr) : SynExpr option =
    match body with
    | SynExpr.YieldOrReturnFrom(expr = BareIdent comp) -> Some comp
    | SynExpr.LetOrUse lou when lou.IsBang && not lou.IsUse ->
        match lou.Bindings, lou.Body with
        | [ SynBinding(headPat = SynPat.Named(ident = SynIdent(ident = v)); expr = BareIdent comp) ],
          SynExpr.YieldOrReturn(expr = SynExpr.Ident returned) when returned.idText = v.idText -> Some comp
        | _ -> None
    | _ -> None

/// `async { return e }` — the returned expression.
[<return: Struct>]
let private (|ReturnOnly|_|) (e: SynExpr) =
    match e with
    | AsyncCe(SynExpr.YieldOrReturn(expr = returned)) -> ValueSome returned
    | _ -> ValueNone

/// Find do-nothing async/task wrappings.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()
    let opens = collectOpens parseTree

    let add (range: range) (replacementText: string) (kind: StripKind) =
        suggestions.Add
            { Range = range
              OriginalText = textOfRange source range
              ReplacementText = replacementText
              Kind = kind }

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path, expr) =
                match expr with
                // async { return e } |> Async.RunSynchronously
                | PipeApp(ReturnOnly returned, RunSynchronously)
                // Async.RunSynchronously (async { return e })
                | SynExpr.App(funcExpr = RunSynchronously; argExpr = SynExpr.Paren(expr = ReturnOnly returned)) when
                    isSingleLine returned.Range && isSafeInline returned
                    ->
                    // parenthesize unless atomic: the strip site can sit inside
                    // a tuple or as an operand of a tighter operator, where a
                    // bare `a + b` or `1, 2` would regroup silently
                    add expr.Range (atomicText source returned) StripKind.WithRunner
                // async { return! comp } / async { let! v = comp in return v }
                | AsyncCe body ->
                    match forwardedComputation body with
                    | Some comp -> add expr.Range (textOfRange source comp.Range) StripKind.Forwarded
                    | None -> ()
                // task { return x }
                | TaskCe(SynExpr.YieldOrReturn(expr = NonThrowing returned)) when
                    opens.Contains "System.Threading.Tasks"
                    ->
                    add
                        expr.Range
                        (sprintf "Task.FromResult(%s)" (textOfRange source returned.Range))
                        StripKind.TaskFromResult
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq suggestions
