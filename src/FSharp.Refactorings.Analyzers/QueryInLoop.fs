/// Refactoring note (performance): iterating an IQueryable inside another
/// loop executes one database query per outer iteration — the N+1 pattern.
///
///     for customer in customers do
///         for order in db.Orders do        // ← a fresh DB round-trip
///             ...                          //   on every customer
///
/// The remedies are design work, so this is advice without a fix:
/// materialize the query once before the loop, push the correlation into a
/// join, or batch the keys.
///
/// Safety rules:
///   - the inner source must statically resolve (via typed check results) to
///     System.Linq.IQueryable — plain sequences and lists never fire
///   - an outer loop whose source involves `chunkBySize` is intentional
///     batching and suppresses the note
///   - the file must have no type errors
module FSharp.Refactorings.QueryInLoop

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

type Suggestion =
    {
        /// The inner `for ... in <queryable>` loop, where the hint anchors.
        Range: range
        /// The queryable source's text, for the message.
        SourceText: string
    }

let private isQueryableName (name: string) =
    name.StartsWith "System.Linq.IQueryable"

[<TailCall>]
let rec private stripAbbreviations (t: FSharpType) =
    if t.HasTypeDefinition && t.TypeDefinition.IsFSharpAbbreviation then
        stripAbbreviations t.TypeDefinition.AbbreviatedType
    else
        t

/// Is the type (after abbreviations) IQueryable or an implementation of it?
let private isQueryableType (t: FSharpType) =
    try
        let t = stripAbbreviations t

        t.HasTypeDefinition
        && (t.TypeDefinition.TryFullName |> Option.exists isQueryableName
            || t.TypeDefinition.AllInterfaces
               |> Seq.exists (fun i ->
                   i.HasTypeDefinition
                   && (i.TypeDefinition.TryFullName |> Option.exists isQueryableName)))
    with _ ->
        false

/// The identifier to resolve for a loop source: `q`, `db.Orders`, `x.A.B`.
[<return: Struct>]
let private (|SourcePathLastIdent|_|) (e: SynExpr) =
    match e with
    | SynExpr.Ident id -> ValueSome id
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> ValueSome(List.last ids)
    | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> ValueSome(List.last ids)
    | _ -> ValueNone

/// Does the source symbol at `ident` have an IQueryable type?
let private resolvesToQueryable (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value ->
            isQueryableType (
                try
                    value.ReturnParameter.Type
                with _ ->
                    value.FullType
            )
        | _ -> false
    | None -> false

/// Find IQueryable iterations nested inside another loop. Requires typed
/// check results; emits nothing when the file has type errors.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        // does the expression subtree call chunkBySize (intentional batching)?
        let mentionsChunking (r: range) =
            index.Exprs
            |> Array.exists (fun (_, e) ->
                match e with
                | SynExpr.Ident id when id.idText = "chunkBySize" -> Range.rangeContainsRange r id.idRange
                | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
                    not ids.IsEmpty && (List.last ids).idText = "chunkBySize"
                    ->
                    Range.rangeContainsRange r e.Range
                | _ -> false)

        [ for path, expr in index.Exprs do
              match expr with
              | SynExpr.ForEach(enumExpr = SourcePathLastIdent sourceId as enumExpr) ->
                  let outerLoops =
                      path
                      |> List.choose (fun node ->
                          match node with
                          | SyntaxNode.SynExpr(SynExpr.ForEach(enumExpr = outerEnum)) -> Some(Some outerEnum)
                          | SyntaxNode.SynExpr(SynExpr.For _)
                          | SyntaxNode.SynExpr(SynExpr.While _) -> Some None
                          | _ -> None)

                  let intentionalBatching =
                      outerLoops
                      |> List.exists (fun enum ->
                          match enum with
                          | Some(outerEnum: SynExpr) -> mentionsChunking outerEnum.Range
                          | None -> false)

                  if
                      not outerLoops.IsEmpty
                      && not intentionalBatching
                      && resolvesToQueryable check source sourceId
                  then
                      { Range = expr.Range
                        SourceText = textOfRange source enumExpr.Range }
              | _ -> () ]
