/// Refactoring: move a List/Seq/Array conversion past the next operation in a
/// pipeline, or drop it when the operation consumes the collection.
///
///     xs |> Seq.toList |> List.map f      →  xs |> Seq.map f |> Seq.toList
///     xs |> List.toArray |> Array.filter f →  xs |> List.filter f |> List.toArray
///     xs |> Seq.toList |> List.length     →  xs |> Seq.length
///     xs |> Seq.toList |> List.iter f     →  xs |> Seq.iter f
///
/// The rewrite avoids building an intermediate collection and is
/// type-correct by construction: the conversion's input type tells us which
/// module's operation accepts the original value, and the trailing conversion
/// (or consuming operation) keeps the overall evaluation eager. Conversions
/// *toward* seq are never moved — that would turn eager code lazy.
///
/// Known (pathological) caveat shared with all such rewrites: if the mapped
/// function mutates the source collection while it is being enumerated, the
/// eager copy made by the conversion masked that; the moved form would throw.
///
/// Safety rules: both pipeline stages single-line; the conversion must be a
/// bare `Module.function`; the operation's head must be exactly the
/// conversion's target module + a whitelisted operation; argument text is
/// preserved verbatim.
module FSharp.Refactorings.ConversionMove

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

type Suggestion =
    {
        /// Range spanning the conversion stage through the operation stage.
        Range: range
        OriginalText: string
        ReplacementText: string
        /// True when the conversion was dropped entirely (consuming operation).
        Eliminated: bool
    }

/// conversion function → (module whose ops accept the conversion's input,
///                        module the conversion produces)
let private conversions =
    dict
        [ "Seq.toList", ("Seq", "List")
          "List.ofSeq", ("Seq", "List")
          "Seq.toArray", ("Seq", "Array")
          "Array.ofSeq", ("Seq", "Array")
          "List.toArray", ("List", "Array")
          "Array.ofList", ("List", "Array")
          "Array.toList", ("Array", "List")
          "List.ofArray", ("Array", "List") ]

/// Operations after which the conversion is still needed (it moves). All are
/// order-preserving element transforms whose Seq/List/Array variants agree
/// once the trailing conversion forces evaluation. `groupBy` is deliberately
/// absent: Seq.groupBy yields seq-valued groups, so the element type changes.
let private movableOps =
    set
        [ "map"
          "mapi"
          "filter"
          "choose"
          "collect"
          "rev"
          "sort"
          "sortBy"
          "sortDescending"
          "sortByDescending"
          "sortWith"
          "distinct"
          "distinctBy"
          "indexed"
          "countBy" ]

/// Operations that consume the collection (the conversion is dropped). The
/// moved form may short-circuit source enumeration (exists/find/head/...),
/// which is the point of the rewrite; results are identical for pure sources.
/// `skip`/`take` are absent: their List and Seq variants throw different
/// exception types on short inputs.
let private consumingOps =
    set
        [ "length"
          "iter"
          "iteri"
          "sum"
          "sumBy"
          "average"
          "averageBy"
          "max"
          "min"
          "maxBy"
          "minBy"
          "exists"
          "forall"
          "isEmpty"
          "fold"
          "reduce"
          "contains"
          "find"
          "tryFind"
          "findIndex"
          "tryFindIndex"
          "pick"
          "tryPick"
          "head"
          "tryHead"
          "last"
          "tryLast"
          // `item` is absent: Array.item throws IndexOutOfRangeException while
          // List.item/Seq.item throw ArgumentException
          "exactlyOne"
          "tryExactlyOne" ]

/// The Array sorts are unstable while the Seq/List sorts are stable, so the
/// sort family must not be moved across an Array boundary.
let private sortFamily =
    set [ "sort"; "sortBy"; "sortDescending"; "sortByDescending"; "sortWith" ]

/// Per-operation restrictions on top of the whitelists: `collect`'s mapper
/// return type differs between modules (only Seq.collect accepts any #seq),
/// and sorting stability differs on Array.
let private opAllowedForModules (opFunc: string) (sourceModule: string) (targetModule: string) =
    if opFunc = "collect" then
        sourceModule = "Seq"
    elif sortFamily.Contains opFunc then
        sourceModule <> "Array" && targetModule <> "Array"
    else
        true

[<return: Struct>]
let private (|ModuleFunc|_|) (e: SynExpr) =
    match e with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ])) -> ValueSome(m.idText, f.idText, e.Range)
    | _ -> ValueNone

/// The leading `Module.function` of a (possibly curried, non-infix)
/// application, e.g. `List.map` in `List.map f`.
[<TailCall>]
let rec private headModuleFunc (e: SynExpr) =
    match e with
    | ModuleFunc(m, f, r) -> Some(m, f, r)
    | SynExpr.App(isInfix = false; funcExpr = funcExpr) -> headModuleFunc funcExpr
    | SynExpr.TypeApp(expr = inner) -> headModuleFunc inner
    | _ -> None

/// Find pipeline segments `conv |> Module.op args` that can be rewritten.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path, expr) =
                match expr with
                | PipeApp(PipeApp(_, (ModuleFunc(convModule, convFunc, _) as convStage)), opStage) when
                    isSingleLine convStage.Range && isSingleLine opStage.Range
                    ->
                    let convKey = convModule + "." + convFunc

                    match conversions.TryGetValue convKey with
                    | true, (sourceModule, targetModule) ->
                        match headModuleFunc opStage with
                        | Some(opModule, opFunc, headRange) when opModule = targetModule ->
                            let movable = movableOps.Contains opFunc
                            let consuming = consumingOps.Contains opFunc

                            if (movable || consuming) && opAllowedForModules opFunc sourceModule targetModule then
                                let argsText =
                                    textOfRange
                                        source
                                        (Range.mkRange opStage.Range.FileName headRange.End opStage.Range.End)

                                let rewrittenOp = sourceModule + "." + opFunc + argsText

                                let replacement =
                                    if consuming then
                                        rewrittenOp
                                    else
                                        rewrittenOp + " |> " + textOfRange source convStage.Range

                                let fullRange =
                                    Range.mkRange convStage.Range.FileName convStage.Range.Start opStage.Range.End

                                suggestions.Add
                                    { Range = fullRange
                                      OriginalText = textOfRange source fullRange
                                      ReplacementText = replacement
                                      Eliminated = consuming }
                        | _ -> ()
                    | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq suggestions
