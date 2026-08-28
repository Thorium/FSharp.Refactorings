/// Diagnostic (correctness): `ignore` applied to an Async value discards the
/// computation without ever running it.
///
///     comp |> ignore      // comp never executes — almost always a bug
///     ignore comp
///
/// No automatic fix is offered: the right repair depends on intent —
/// `do! comp |> Async.Ignore` awaits and discards the result,
/// `Async.Start comp` fires and forgets — and only the author knows which.
///
/// Typed rule: the operand must be a simple identifier whose type resolves to
/// FSharp.Core's Async<'T> (shadowing-proof); the file must have no type
/// errors.
module FSharp.Refactor.AsyncIgnore

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactor.Text

type Suggestion =
    {
        Range: range
        OriginalText: string
        /// The discarded computation's name, for the message.
        Name: string
    }

[<Literal>]
let private AsyncTypeName = "Microsoft.FSharp.Control.FSharpAsync`1"

/// `ignore x` / `x |> ignore` — the operand, parens stripped.
[<return: Struct>]
let private (|Ignored|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = IdentName "ignore"; argExpr = arg) -> ValueSome(stripParens arg)
    | PipeApp(arg, IdentName "ignore") -> ValueSome(stripParens arg)
    | _ -> ValueNone

/// The head identifier of an application spine and how many arguments were
/// applied to it: `f a b` → (f, 2); `x.M(a)` → (M, 1); `x |> g` → (g, 1).
/// Real fire-and-forget bugs are written as a direct call ignored —
/// `saveUserAsync user |> ignore` — almost never as a named binding.
let rec private headAndDepth (depth: int) (e: SynExpr) =
    match e with
    | SynExpr.Paren(expr = inner) -> headAndDepth depth inner
    // a pipe IS an App(isInfix = false) at the outer node, so this arm must
    // come first or `x |> makeAsync` dead-ends in the operator application
    | PipeApp(_, rhs) -> headAndDepth (depth + 1) rhs
    | SynExpr.App(isInfix = false; funcExpr = f) -> headAndDepth (depth + 1) f
    | SynExpr.TypeApp(expr = inner) -> headAndDepth depth inner
    | SynExpr.Ident id -> ValueSome(id, depth)
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))
    | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> ValueSome(List.last ids, depth)
    | _ -> ValueNone

/// Find Async values discarded with plain ignore.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()

    let isAsyncType (t: FSharpType) =
        try
            let t = OptionModule.stripAbbreviations t
            t.HasTypeDefinition && t.TypeDefinition.TryFullName = Some AsyncTypeName
        with _ ->
            false

    let resolve (ident: Ident) =
        let r = ident.idRange
        let lineText = source.GetLineString(r.EndLine - 1)

        match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
        | Some symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as value -> ValueSome value
            | _ -> ValueNone
        | None -> ValueNone

    // a bare name: its own type is Async. An application: the head must be
    // FULLY applied (partial application ignores a function, a different
    // mistake) and its final return type Async.
    let ignoredAsync (operand: SynExpr) =
        match operand with
        | SynExpr.Ident ident ->
            match resolve ident with
            | ValueSome value when isAsyncType value.FullType -> ValueSome ident
            | _ -> ValueNone
        | SynExpr.App _ ->
            match headAndDepth 0 operand with
            | ValueSome(headIdent, applied) when applied > 0 ->
                match resolve headIdent with
                | ValueSome value ->
                    // groups must be KNOWN and consumed: a function-typed
                    // PARAMETER reports zero groups while its ReturnParameter
                    // may still be the final Async — trusting that would flag
                    // a partial application of it
                    let fullyApplied =
                        try
                            let groups = value.CurriedParameterGroups.Count
                            groups > 0 && groups <= applied
                        with _ ->
                            false

                    if fullyApplied && isAsyncType value.ReturnParameter.Type then
                        ValueSome headIdent
                    else
                        ValueNone
                | ValueNone -> ValueNone
            | _ -> ValueNone
        | _ -> ValueNone

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path, expr) =
                match expr with
                | Ignored operand ->
                    match ignoredAsync operand with
                    | ValueSome ident ->
                        suggestions.Add
                            { Range = expr.Range
                              OriginalText = textOfRange source expr.Range
                              Name = ident.idText }
                    | ValueNone -> ()
                | _ -> () }

    if OptionModule.hasErrors check then
        []
    else
        AstIndex.replay collector parseTree
        List.ofSeq suggestions
