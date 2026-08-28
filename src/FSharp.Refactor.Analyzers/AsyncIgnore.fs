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

/// `ignore x` / `x |> ignore` with x a bare identifier.
[<return: Struct>]
let private (|IgnoredIdent|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = IdentName "ignore"; argExpr = SynExpr.Ident ident) -> ValueSome ident
    | PipeApp(SynExpr.Ident ident, IdentName "ignore") -> ValueSome ident
    | _ -> ValueNone

/// Find Async values discarded with plain ignore.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()

    let isAsyncIdent (ident: Ident) =
        let r = ident.idRange
        let lineText = source.GetLineString(r.EndLine - 1)

        match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
        | Some symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as value ->
                let t = OptionModule.stripAbbreviations value.FullType

                t.HasTypeDefinition && t.TypeDefinition.TryFullName = Some AsyncTypeName
            | _ -> false
        | None -> false

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path, expr) =
                match expr with
                | IgnoredIdent ident when isAsyncIdent ident ->
                    suggestions.Add
                        { Range = expr.Range
                          OriginalText = textOfRange source expr.Range
                          Name = ident.idText }
                | _ -> () }

    if OptionModule.hasErrors check then
        []
    else
        AstIndex.replay collector parseTree
        List.ofSeq suggestions
