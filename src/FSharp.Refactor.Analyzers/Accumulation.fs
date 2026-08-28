/// Two accumulation refactorings:
///
/// 1. Mutable accumulator loop → fold/sum (FR0050, fix):
///
///        let mutable total = 0                let total = xs |> Seq.sum
///        for x in xs do               →
///            total <- total + x
///
///    The general shape becomes `Seq.fold (fun acc x -> ...) init xs` —
///    the same expression evaluated with the same bindings in the same
///    order, so the rewrite is behavior-preserving; `Seq.sum`/`sumBy`
///    specializations fire when the combine is FSharp.Core's `+` over a
///    zero initializer.
///
/// 2. Quadratic append in a loop (FR0051, note): `acc <- acc @ [x]` or
///    `acc <- Array.append acc [| x |]` copies the accumulator on every
///    iteration — O(n²). Accumulate into a ResizeArray, or cons (`::`)
///    and `List.rev` once at the end.
module FSharp.Refactor.Accumulation

open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type FoldSuggestion =
    { Range: range
      OriginalText: string
      ReplacementText: string }

type QuadraticSuggestion =
    {
        Range: range
        /// The accumulator's name, for the message.
        Name: string
    }

/// A loop pattern usable as a lambda parameter.
let private lambdaPatText (source: ISourceText) (pat: SynPat) =
    let text = textOfRange source pat.Range

    match pat with
    | SynPat.Named _
    | SynPat.Wild _
    | SynPat.Paren _ -> ValueSome text
    | SynPat.Tuple _ -> ValueSome $"({text})"
    | _ -> ValueNone

let private isZeroLike (e: SynExpr) =
    match e with
    | SynExpr.Const(SynConst.Int32 0, _)
    | SynExpr.Const(SynConst.Double 0.0, _) -> true
    | _ -> false

// does any expression inside `r` mention `name`?
let private mentionsIn (index: AstIndex.Index) (name: string) (r: range) =
    index.Exprs
    |> Array.exists (fun (_, e) ->
        match e with
        | SynExpr.Ident id when id.idText = name -> Range.rangeContainsRange r id.idRange
        | SynExpr.LongIdent(longDotId = SynLongIdent(id = firstId :: _)) when firstId.idText = name ->
            Range.rangeContainsRange r firstId.idRange
        | _ -> false)

let private insideLoop (path: SyntaxNode list) =
    path
    |> List.exists (fun node ->
        match node with
        | SyntaxNode.SynExpr(SynExpr.For _)
        | SyntaxNode.SynExpr(SynExpr.ForEach _)
        | SyntaxNode.SynExpr(SynExpr.While _) -> true
        | _ -> false)

/// Find both accumulation shapes. Requires typed check results (the
/// sum/sumBy specialization checks that `+` is FSharp.Core's).
let find
    (parseTree: ParsedInput)
    (source: ISourceText)
    (check: FSharpCheckFileResults)
    : FoldSuggestion list * QuadraticSuggestion list =
    let folds = ResizeArray<FoldSuggestion>()
    let quadratics = ResizeArray<QuadraticSuggestion>()

    if OptionModule.hasErrors check then
        [], []
    else
        let index = AstIndex.ofTree parseTree

        for path, expr in index.Exprs do
            match expr with
            // FR0050: let mutable acc = init; for pat in src do acc <- rhs; rest
            | SynExpr.LetOrUse lou when
                not (lou.IsBang || lou.IsUse)
                && (match lou.Bindings with
                    | [ SynBinding(isMutable = true) ] -> true
                    | _ -> false)
                ->
                match lou.Bindings, lou.Body with
                | [ SynBinding(headPat = SynPat.Named(ident = SynIdent(ident = acc)); expr = init) ],
                  SynExpr.Sequential(
                      expr1 = SynExpr.ForEach(pat = pat; enumExpr = src; bodyExpr = loopBody) as forEach; expr2 = rest) ->
                    let loopBody =
                        match loopBody with
                        | SynExpr.Do(expr = inner) -> inner
                        | other -> other

                    match loopBody with
                    | SynExpr.LongIdentSet(SynLongIdent(id = [ target ]), rhs, _) when
                        target.idText = acc.idText
                        && isSingleLine rhs.Range
                        && isSingleLine src.Range
                        && isSingleLine init.Range
                        && isSafeInline rhs
                        // acc must not be re-assigned in the continuation
                        && not (
                            Regex.IsMatch(textOfRange source rest.Range, @"\b" + Regex.Escape acc.idText + @"\b\s*<-")
                        )
                        ->
                        match lambdaPatText source pat with
                        | ValueSome patText ->
                            let srcText = atomicText source src

                            let replacementBody =
                                match rhs with
                                // acc + e with FSharp.Core's (+) over zero
                                | SynExpr.App(
                                    funcExpr = SynExpr.App(funcExpr = SingleIdent op; argExpr = SynExpr.Ident lhsId)
                                    argExpr = e) when
                                    op.idText = "op_Addition"
                                    && lhsId.idText = acc.idText
                                    && isZeroLike init
                                    && not (mentionsIn index acc.idText e.Range)
                                    && OptionModule.resolvesToCoreOperator check source op
                                    ->
                                    match stripParens e with
                                    | SynExpr.Ident v when (patBoundNames pat |> List.tryExactlyOne) = Some v.idText ->
                                        $"{srcText} |> Seq.sum"
                                    | _ -> $"{srcText} |> Seq.sumBy (fun {patText} -> {textOfRange source e.Range})"
                                // acc + e over a STRING accumulator: a fold
                                // of (+) would still be O(n²) in allocations,
                                // String.concat builds the result once
                                | SynExpr.App(
                                    funcExpr = SynExpr.App(funcExpr = SingleIdent op; argExpr = SynExpr.Ident lhsId)
                                    argExpr = e) when
                                    op.idText = "op_Addition"
                                    && lhsId.idText = acc.idText
                                    && (match stripParens init with
                                        | SynExpr.Const(SynConst.String _, _) -> true
                                        | _ -> false)
                                    && not (mentionsIn index acc.idText e.Range)
                                    && OptionModule.resolvesToCoreOperator check source op
                                    ->
                                    let mapped =
                                        match stripParens e with
                                        | SynExpr.Ident v when (patBoundNames pat |> List.tryExactlyOne) = Some v.idText ->
                                            srcText
                                        | _ -> $"{srcText} |> Seq.map (fun {patText} -> {textOfRange source e.Range})"

                                    let concatenated = $"{mapped} |> String.concat \"\""

                                    match stripParens init with
                                    | SynExpr.Const(SynConst.String("", _, _), _) -> concatenated
                                    | _ -> $"{atomicText source init} + ({concatenated})"
                                | _ ->
                                    $"{srcText} |> Seq.fold (fun {acc.idText} {patText} -> {textOfRange source rhs.Range}) {atomicText source init}"

                            // cover the whole `let mutable` binding + loop
                            let editRange = Range.mkRange expr.Range.FileName expr.Range.Start forEach.Range.End

                            if not (spansDirective source editRange) then
                                folds.Add
                                    { Range = editRange
                                      OriginalText = textOfRange source editRange
                                      ReplacementText = $"let {acc.idText} = {replacementBody}" }
                        | ValueNone -> ()
                    | _ -> ()
                | _ -> ()
            // FR0051: quadratic append inside a loop
            | SynExpr.LongIdentSet(SynLongIdent(id = [ acc ]), rhs, _) when insideLoop path ->
                let quadratic =
                    match rhs with
                    | SynExpr.App(
                        funcExpr = SynExpr.App(funcExpr = SingleIdent op; argExpr = SynExpr.Ident lhsId); argExpr = _) when
                        op.idText = "op_Append" && lhsId.idText = acc.idText
                        ->
                        true
                    | SynExpr.App(
                        funcExpr = SynExpr.App(
                            funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ])); argExpr = a1)
                        argExpr = a2) when
                        m.idText = "Array"
                        && f.idText = "append"
                        && ((match a1 with
                             | SynExpr.Ident i -> i.idText = acc.idText
                             | _ -> false)
                            || (match a2 with
                                | SynExpr.Ident i -> i.idText = acc.idText
                                | _ -> false))
                        ->
                        true
                    | _ -> false

                if quadratic then
                    quadratics.Add
                        { Range = expr.Range
                          Name = acc.idText }
            | _ -> ()

        List.ofSeq folds, List.ofSeq quadratics
