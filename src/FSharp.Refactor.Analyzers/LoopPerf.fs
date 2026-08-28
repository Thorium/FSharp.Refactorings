/// Two loop-performance notes (advice only — both remedies change types
/// or structure, which is the author's call):
///
/// 1. Linear probe per iteration (FR0035): `List.contains x ys` inside a
///    loop — or inside a callback given to a List/Seq/Array function —
///    scans `ys` linearly every time. Building a Set once outside makes
///    each probe O(log n):
///
///        let ySet = Set.ofList ys
///        xs |> List.filter (fun x -> ySet.Contains x)
///
/// 2. Expensive construction per iteration (FR0037): some types are
///    expensive by design and meant to be built once — ConcurrentDictionary
///    (its documentation recommends few, long-lived instances),
///    JsonSerializerOptions (CA1869: caching it is the single biggest
///    System.Text.Json perf lever), and SearchValues.Create (CA1870: the
///    whole point is amortizing the precomputation). Constructing one
///    inside a loop defeats them; hoist it outside or make it static.
///
/// Both only fire when the probed collection / constructed value is
/// loop-invariant as far as the syntax shows: a probe of the loop variable
/// itself is never flagged.
module FSharp.Refactor.LoopPerf

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type ContainsSuggestion =
    {
        Range: range
        /// The linearly probed collection, for the message.
        CollectionName: string
        /// "List", "Array", or "Seq", for the message.
        ModuleName: string
    }

type ConstructionSuggestion =
    {
        Range: range
        /// The constructed type's name, for the message.
        TypeName: string
    }

let private collectionModules = set [ "List"; "Array"; "Seq" ]

/// Types whose construction inside a loop is expensive by design.
///
/// `Regex` belongs here as much as the others: constructing one parses and
/// compiles the pattern, which is the whole cost. FR0015 covers the STATIC
/// calls — `Regex.IsMatch(s, "...")` in a loop — but a `Regex` bound to a
/// value inside a loop went unnoticed by either rule.
let private expensiveTypes =
    set [ "ConcurrentDictionary"; "JsonSerializerOptions"; "Regex" ]

/// Static factories with the same build-once intent (Type, method).
let private expensiveFactories = [ "SearchValues", "Create" ]

/// `<m>.contains item coll` and `coll |> <m>.contains item`.
[<return: Struct>]
let private (|ContainsCall|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(
        isInfix = false
        funcExpr = SynExpr.App(
            isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ])); argExpr = _)
        argExpr = SynExpr.Ident coll) when collectionModules.Contains m.idText && f.idText = "contains" ->
        ValueSome(m.idText, coll)
    | PipeApp(SynExpr.Ident coll,
              SynExpr.App(
                  isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ])); argExpr = _)) when
        collectionModules.Contains m.idText && f.idText = "contains"
        ->
        ValueSome(m.idText, coll)
    | _ -> ValueNone

/// `new ConcurrentDictionary<...>(...)` / `ConcurrentDictionary<...>(...)`
/// / `ConcurrentDictionary(...)` — the constructed expensive type's name.
[<return: Struct>]
let private (|ExpensiveCtor|_|) (e: SynExpr) =
    let typeNameOf (t: SynType) =
        match t with
        | SynType.App(typeName = SynType.LongIdent(SynLongIdent(id = ids)))
        | SynType.LongIdent(SynLongIdent(id = ids)) when
            not ids.IsEmpty && expensiveTypes.Contains (List.last ids).idText
            ->
            ValueSome (List.last ids).idText
        | _ -> ValueNone

    let ctorNameOf (inner: SynExpr) =
        match inner with
        | SynExpr.Ident id when expensiveTypes.Contains id.idText -> ValueSome id.idText
        | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
            not ids.IsEmpty && expensiveTypes.Contains (List.last ids).idText
            ->
            ValueSome (List.last ids).idText
        | _ -> ValueNone

    let factoryNameOf (inner: SynExpr) =
        match inner with
        | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when ids.Length >= 2 ->
            let typeId = ids.[ids.Length - 2].idText
            let methodId = (List.last ids).idText

            if expensiveFactories |> List.contains (typeId, methodId) then
                ValueSome typeId
            else
                ValueNone
        | _ -> ValueNone

    match e with
    | SynExpr.New(targetType = t) -> typeNameOf t
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.TypeApp(expr = inner)) -> ctorNameOf inner
    | SynExpr.App(isInfix = false; funcExpr = (SynExpr.Ident _ | SynExpr.LongIdent _) as inner) ->
        match ctorNameOf inner with
        | ValueSome name -> ValueSome name
        | ValueNone -> factoryNameOf inner
    | _ -> ValueNone

/// The loop-context binders along the path: Some names when the node sits
/// inside a loop or a collection-function callback, None otherwise.
let private loopBinders (path: SyntaxNode list) =
    let mutable insideLoop = false
    let binders = ResizeArray<string>()
    let mutable sawLambda = false

    for node in path do
        match node with
        | SyntaxNode.SynExpr(SynExpr.ForEach(pat = p)) ->
            insideLoop <- true
            binders.AddRange(patBoundNames p)
        | SyntaxNode.SynExpr(SynExpr.For(ident = loopVar)) ->
            insideLoop <- true
            binders.Add loopVar.idText
        | SyntaxNode.SynExpr(SynExpr.While _) -> insideLoop <- true
        // a let between the loop and the probe may rebind the collection
        // per iteration — its bindings are loop-local, not loop-invariant
        | SyntaxNode.SynExpr(SynExpr.LetOrUse lou) ->
            for SynBinding(headPat = p) in lou.Bindings do
                binders.AddRange(patBoundNames p)
        | SyntaxNode.SynExpr(SynExpr.Lambda(parsedData = parsedData)) ->
            sawLambda <- true

            match parsedData with
            | Some(pats, _) ->
                for p in pats do
                    binders.AddRange(patBoundNames p)
            | None -> ()
        | SyntaxNode.SynExpr(SynExpr.App(funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = m :: _)))) when
            sawLambda && collectionModules.Contains m.idText
            ->
            // the lambda is a callback of a collection function: it runs
            // once per element, which is a loop
            insideLoop <- true
        | _ -> ()

    if insideLoop then
        ValueSome(Set.ofSeq binders)
    else
        ValueNone

/// Find per-iteration linear probes and expensive constructions.
let find (parseTree: ParsedInput) (source: ISourceText) : ContainsSuggestion list * ConstructionSuggestion list =
    ignore source
    let index = AstIndex.ofTree parseTree
    let contains = ResizeArray<ContainsSuggestion>()
    let constructions = ResizeArray<ConstructionSuggestion>()

    for path, expr in index.Exprs do
        match expr with
        | ContainsCall(moduleName, coll) ->
            match loopBinders path with
            | ValueSome binders when not (binders.Contains coll.idText) ->
                contains.Add
                    { Range = expr.Range
                      CollectionName = coll.idText
                      ModuleName = moduleName }
            | _ -> ()
        | ExpensiveCtor typeName ->
            match loopBinders path with
            | ValueSome _ ->
                constructions.Add
                    { Range = expr.Range
                      TypeName = typeName }
            | ValueNone -> ()
        | _ -> ()

    List.ofSeq contains, List.ofSeq constructions
