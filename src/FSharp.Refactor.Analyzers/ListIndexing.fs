/// Diagnostic (performance): positional indexing into an F# LIST inside a
/// loop.
///
///     let names : string list = ...
///     for i in 0 .. count - 1 do
///         printfn "%s" names.[i]      // each access walks i cons cells
///
/// `xs.[i]` looks like an array access and is O(i) on a list — in a loop
/// that is the quietest quadratic in F#, and the shape LLMs produce
/// constantly because `[ ]` literals make lists which they then index like
/// Python lists. Advice only: the right repair is iterating directly (the
/// canonical `for i in 0 .. xs.Length - 1` shape gets an automatic fix
/// from FR0101), or converting once with List.toArray when random access
/// is really needed.
///
/// Typed rule: the receiver must resolve to FSharpList — arrays,
/// ResizeArray and dictionaries share the same syntax and are fine. The
/// `List.item`/`List.nth` spellings pin the type by module name. Constant
/// indexes are skipped (`xs.[0]` is a deliberate head access), and so is a
/// receiver bound inside the loop (a fresh short list per iteration is a
/// different story).
module FSharp.Refactor.ListIndexing

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        Range: range
        /// The indexed list's source text, for the message.
        CollectionText: string
    }

[<return: Struct>]
let private (|Path|_|) (e: SynExpr) =
    match e with
    | SynExpr.Ident id -> ValueSome(id, id, id.idText)
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty ->
        ValueSome(List.head ids, List.last ids, identText ids)
    | _ -> ValueNone

let private isConstIndex (e: SynExpr) =
    match stripParens e with
    | SynExpr.Const _ -> true
    | _ -> false

/// Find list indexing inside loops. Requires typed check results for the
/// receiver's type.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        let resolvesToList (ident: Ident) =
            let r = ident.idRange
            let lineText = source.GetLineString(r.EndLine - 1)

            let rec stripInstance (t: FSharpType) =
                if t.IsAbbreviation then stripInstance t.AbbreviatedType else t

            let isListType (t: FSharpType) =
                try
                    let t = stripInstance t

                    t.HasTypeDefinition
                    && t.TypeDefinition.TryFullName = Some "Microsoft.FSharp.Collections.FSharpList`1"
                with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                    false

            match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
            | Some symbolUse ->
                match symbolUse.Symbol with
                | :? FSharpMemberOrFunctionOrValue as value ->
                    isListType (
                        try
                            value.ReturnParameter.Type
                        with _ ->
                            value.FullType
                    )
                | :? FSharpField as field -> isListType field.FieldType
                | _ -> false
            | None -> false

        [ for path, expr in index.Exprs do
              let candidate =
                  match expr with
                  // xs.[i] and the F#6 xs[i]
                  | SynExpr.DotIndexedGet(objectExpr = Path(root, last, text); indexArgs = idx) when
                      not (isConstIndex idx)
                      ->
                      Some(root, last, text, true)
                  | SynExpr.App(
                      flag = ExprAtomicFlag.Atomic
                      funcExpr = Path(root, last, text)
                      argExpr = SynExpr.ArrayOrListComputed(expr = idx)) when not (isConstIndex idx) ->
                      Some(root, last, text, true)
                  // List.item i xs / xs |> List.item i (nth likewise) —
                  // the module name pins the type, no resolution needed
                  | SynExpr.App(
                      funcExpr = SynExpr.App(
                          funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ])); argExpr = idx)
                      argExpr = Path(root, last, text)) when
                      m.idText = "List"
                      && (f.idText = "item" || f.idText = "nth")
                      && not (isConstIndex idx)
                      ->
                      Some(root, last, text, false)
                  | PipeApp(Path(root, last, text),
                            SynExpr.App(
                                funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ])); argExpr = idx)) when
                      m.idText = "List"
                      && (f.idText = "item" || f.idText = "nth")
                      && not (isConstIndex idx)
                      ->
                      Some(root, last, text, false)
                  | _ -> None

              match candidate with
              | Some(root, last, text, needsTypeProof) ->
                  match LoopPerf.loopBinders path with
                  | ValueSome binders when
                      not (binders.Contains root.idText)
                      && (not needsTypeProof || resolvesToList last)
                      ->
                      { Range = expr.Range
                        CollectionText = text }
                  | _ -> ()
              | None -> () ]
