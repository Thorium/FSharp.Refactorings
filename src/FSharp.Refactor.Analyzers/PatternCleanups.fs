/// Three pattern-level cleanups in the ReSharper tradition:
///
/// 1. Cons of empty (FR0087): the pattern `x :: []` is `[ x ]`.
/// 2. All-wildcard case fields (FR0088): `Case(_, _)` matches exactly
///    what `Case _` matches; the field arity is noise. Typed-gated to
///    real union cases — a parameterized active pattern's arguments are
///    not field patterns.
/// 3. Tuple in a list literal (FR0089, note): `[ 1, 2 ]` is a
///    single-tuple list — `,` builds a tuple, `;` separates elements.
///    The classic paste-from-C# trap; advice only, single-tuple lists are
///    sometimes intended.
module FSharp.Refactor.PatternCleanups

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type ConsSuggestion =
    { Range: range
      OriginalText: string
      ReplacementText: string }

type WildFieldsSuggestion =
    { Range: range
      OriginalText: string
      ReplacementText: string
      CaseName: string }

type TupleInListSuggestion =
    {
        /// The editor's fix: the elements separated by `;`.
        Fix: range * string * string
        Range: range
        /// Element count of the accidental tuple.
        Elements: int
    }

let private resolvesToUnionCase (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpUnionCase -> true
        | _ -> false
    | None -> false

/// Find all three. Requires typed check results for the union-case gate.
let find
    (parseTree: ParsedInput)
    (source: ISourceText)
    (check: FSharpCheckFileResults)
    : ConsSuggestion list * WildFieldsSuggestion list * TupleInListSuggestion list =
    let index = AstIndex.ofTree parseTree
    let conses = ResizeArray<ConsSuggestion>()
    let wilds = ResizeArray<WildFieldsSuggestion>()
    let tuples = ResizeArray<TupleInListSuggestion>()
    let hasErrors = OptionModule.hasErrors check

    for _, p in index.Pats do
        match p with
        // FR0087: x :: []
        | SynPat.ListCons(lhsPat = lhs; rhsPat = SynPat.ArrayOrList(_, [], _)) when
            isSingleLine p.Range && not ((textOfRange source lhs.Range).Contains ';')
            ->
            conses.Add
                { Range = p.Range
                  OriginalText = textOfRange source p.Range
                  ReplacementText = $"[ {textOfRange source lhs.Range} ]" }
        // FR0088: Case(_, _) — every field a wildcard
        | SynPat.LongIdent(longDotId = SynLongIdent(id = ids); argPats = SynArgPats.Pats [ SynPat.Paren(inner, _) ]) when
            not ids.IsEmpty
            && not hasErrors
            && (match inner with
                | SynPat.Tuple(elementPats = elems) when elems.Length >= 2 ->
                    elems
                    |> List.forall (fun e ->
                        match e with
                        | SynPat.Wild _ -> true
                        | _ -> false)
                | SynPat.Wild _ -> true
                | _ -> false)
            && resolvesToUnionCase check source (List.last ids)
            ->
            let caseEnd = (List.last ids).idRange.End
            let editRange = Range.mkRange p.Range.FileName caseEnd p.Range.End

            wilds.Add
                { Range = editRange
                  OriginalText = textOfRange source editRange
                  ReplacementText = " _"
                  CaseName = (List.last ids).idText }
        | _ -> ()

    // FR0089: [ 1, 2 ] — the whole literal is one tuple. Only ALL-NUMERIC
    // tuples fire: that is the paste-trap shape, while a single tuple of
    // expressions ([ range, text, code ]) or of strings
    // ([ "SearchValues", "Create" ]) is a deliberate one-element table
    // `grid[0, 1, 2]` is INDEXING, not a literal. Since F# 6 that spells
    // as an ATOMIC application of a bracket to the thing before it — the
    // same parse shape as a list — so the atomic flag is what separates
    // them (`f [1; 2]`, with a space, is a real argument and NonAtomic).
    // The `.[ ]` spelling never reached here; the modern one it
    // recommends did, and TorchSharp code is nothing but multi-dimensional
    // indexing: 6 false notes in Fuuga's EvalTests alone.
    let inIndexPosition (path: SyntaxNode list) (e: SynExpr) =
        match path with
        | SyntaxNode.SynExpr(SynExpr.App(flag = ExprAtomicFlag.Atomic; argExpr = arg)) :: _ -> arg.Range = e.Range
        | _ -> false

    for path, e in index.Exprs do
        match e with
        | SynExpr.ArrayOrListComputed(expr = SynExpr.Tuple(isStruct = false; exprs = elems)) when
            not (inIndexPosition path e)
            && elems.Length >= 2
            && elems
               |> List.forall (fun el ->
                   match el with
                   | SynExpr.Const(SynConst.Int32 _, _)
                   | SynExpr.Const(SynConst.Int64 _, _)
                   | SynExpr.Const(SynConst.Double _, _)
                   | SynExpr.Const(SynConst.Single _, _)
                   | SynExpr.Const(SynConst.Decimal _, _) -> true
                   | _ -> false)
            ->
            // the editor's fix: the same elements separated by `;` — the
            // list the author most likely meant
            let original = textOfRange source e.Range

            let opening, closing =
                if original.StartsWith "[|" then
                    "[| ", " |]"
                else
                    "[ ", " ]"

            let separated =
                opening
                + (elems |> List.map (fun el -> textOfRange source el.Range) |> String.concat "; ")
                + closing

            tuples.Add
                { Fix = (e.Range, original, separated)
                  Range = e.Range
                  Elements = elems.Length }
        | _ -> ()

    List.ofSeq conses, List.ofSeq wilds, List.ofSeq tuples
