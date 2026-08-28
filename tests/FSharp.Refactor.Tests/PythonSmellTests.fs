module FSharp.Refactor.Tests.PythonSmellTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

// ---- FR0101 IndexedLoop ----

let private indexedLoopsIn (source: string) =
    let tree, sourceText = parse source
    IndexedLoop.find tree sourceText

let private applyAll (source: string) (edits: (FSharp.Compiler.Text.range * string * string) list) =
    edits
    |> List.sortByDescending (fun (r, _, _) -> r.StartLine, r.StartColumn)
    |> List.fold (fun acc (r, _, replacement) -> applyEdit acc r replacement) source

let private assertIndexedLoop (source: string) (expectedPatched: string) =
    match indexedLoopsIn source with
    | [ s ] ->
        let patched = applyAll source s.Edits
        Assert.Equal(expectedPatched, patched)
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one indexed-loop fix, got %A" other

[<Fact>]
let ``the canonical range-over-length loop iterates directly`` () =
    assertIndexedLoop
        "module Test\nlet f (xs: int[]) =\n    for i in 0 .. xs.Length - 1 do\n        printfn \"%d\" xs.[i]"
        "module Test\nlet f (xs: int[]) =\n    for x in xs do\n        printfn \"%d\" x"

[<Fact>]
let ``the F#6 indexer spelling converts too`` () =
    assertIndexedLoop
        "module Test\nlet f (xs: int[]) =\n    for i in 0 .. xs.Length - 1 do\n        printfn \"%d\" xs[i]"
        "module Test\nlet f (xs: int[]) =\n    for x in xs do\n        printfn \"%d\" x"

[<Fact>]
let ``the module-length spelling converts too`` () =
    assertIndexedLoop
        "module Test\nlet f (xs: int[]) =\n    for i in 0 .. Array.length xs - 1 do\n        printfn \"%d\" xs.[i]"
        "module Test\nlet f (xs: int[]) =\n    for x in xs do\n        printfn \"%d\" x"

[<Fact>]
let ``an index also used as a value is the author's call`` () =
    // iteri would fit, but that changes shape — stay quiet
    Assert.Empty(
        indexedLoopsIn
            "module Test\nlet f (xs: int[]) =\n    for i in 0 .. xs.Length - 1 do\n        printfn \"%d %d\" i xs.[i]"
    )

[<Fact>]
let ``element writes need the index`` () =
    Assert.Empty(
        indexedLoopsIn
            "module Test\nlet f (xs: int[]) =\n    for i in 0 .. xs.Length - 1 do\n        xs.[i] <- xs.[i] + 1"
    )

[<Fact>]
let ``a bound over a different collection is left alone`` () =
    Assert.Empty(
        indexedLoopsIn
            "module Test\nlet f (xs: int[]) (ys: int[]) =\n    for i in 0 .. xs.Length - 1 do\n        printfn \"%d\" ys.[i]"
    )

// ---- FR0102 ListIndexing ----

let private listIndexingIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    ListIndexing.find tree sourceText checkResults

[<Fact>]
let ``indexing a list inside a loop is quadratic and noted`` () =
    let suggestions =
        listIndexingIn
            "let f (names: string list) (count: int) =\n    for i in 0 .. count - 1 do\n        printfn \"%s\" names.[i]"

    match suggestions with
    | [ s ] -> Assert.Equal("names", s.CollectionText)
    | other -> failwithf "Expected exactly one list-indexing note, got %A" other

[<Fact>]
let ``indexing an array is what arrays are for`` () =
    Assert.Empty(
        listIndexingIn
            "let f (names: string[]) (count: int) =\n    for i in 0 .. count - 1 do\n        printfn \"%s\" names.[i]"
    )

[<Fact>]
let ``List.item in a collection callback is a loop too`` () =
    let suggestions =
        listIndexingIn
            "let f (names: string list) (idxs: int list) =\n    idxs |> List.iter (fun i -> printfn \"%s\" (List.item i names))"

    match suggestions with
    | [ s ] -> Assert.Equal("names", s.CollectionText)
    | other -> failwithf "Expected exactly one List.item note, got %A" other

[<Fact>]
let ``a single indexed access outside any loop is fine`` () =
    Assert.Empty(listIndexingIn "let f (names: string list) (i: int) = names.[i]")

// ---- FR0103 TypeTestChain ----

let private typeTestsIn (source: string) =
    let tree, sourceText = parse source
    TypeTestChain.find tree sourceText

let private assertTypeTestFix (source: string) (expectedReplacement: string) =
    match typeTestsIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one type-test-chain fix, got %A" other

[<Fact>]
let ``the isinstance ladder becomes a match`` () =
    assertTypeTestFix
        "module Test\ntype Circle() = member _.R = 1.0\ntype Rect() = member _.W = 2.0\nlet f (shape: obj) =\n    if (shape :? Circle) then (shape :?> Circle).R\n    elif (shape :? Rect) then (shape :?> Rect).W\n    else 0.0"
        "match shape with | :? Circle as v -> v.R | :? Rect as v -> v.W | _ -> 0.0"

[<Fact>]
let ``a branch without a cast just drops the as-binder`` () =
    assertTypeTestFix
        "module Test\ntype Circle() = member _.R = 1.0\ntype Rect() = member _.W = 2.0\nlet f (shape: obj) =\n    if (shape :? Circle) then 1.0\n    elif (shape :? Rect) then (shape :?> Rect).W\n    else 0.0"
        "match shape with | :? Circle -> 1.0 | :? Rect as v -> v.W | _ -> 0.0"

[<Fact>]
let ``a compound condition needs a when guard and stays`` () =
    Assert.Empty(
        typeTestsIn
            "module Test\ntype Circle() = member _.R = 1.0\ntype Rect() = member _.W = 2.0\nlet f (shape: obj) (big: bool) =\n    if (shape :? Circle) && big then 1.0\n    elif (shape :? Rect) then 2.0\n    else 0.0"
    )

[<Fact>]
let ``a cast to a different type means the author knows more`` () =
    Assert.Empty(
        typeTestsIn
            "module Test\ntype Circle() = member _.R = 1.0\ntype Rect() = member _.W = 2.0\nlet f (shape: obj) =\n    if (shape :? Circle) then (shape :?> Rect).W\n    elif (shape :? Rect) then 2.0\n    else 0.0"
    )

[<Fact>]
let ``a single type test reads fine as an if`` () =
    Assert.Empty(
        typeTestsIn
            "module Test\ntype Circle() = member _.R = 1.0\nlet f (shape: obj) =\n    if (shape :? Circle) then 1.0 else 0.0"
    )

// ---- FR0104 RecursiveAppend ----

let private recursiveAppendsIn (source: string) =
    let tree, sourceText = parse source
    RecursiveAppend.find tree sourceText

[<Fact>]
let ``a singleton append per recursive call is noted`` () =
    let suggestions =
        recursiveAppendsIn
            "module Test\nlet rec collect (keep: int -> bool) acc xs =\n    match xs with\n    | [] -> acc\n    | x :: rest when keep x -> collect keep (acc @ [ x ]) rest\n    | _ :: rest -> collect keep acc rest"

    match suggestions with
    | [ s ] ->
        Assert.Equal("collect", s.FunctionName)
        Assert.Equal("acc", s.AccumulatorName)
    | other -> failwithf "Expected exactly one recursive-append note, got %A" other

[<Fact>]
let ``the List.append spelling is noted too`` () =
    let suggestions =
        recursiveAppendsIn
            "module Test\nlet rec collect acc xs =\n    match xs with\n    | [] -> acc\n    | x :: rest -> collect (List.append acc [ x ]) rest"

    match suggestions with
    | [ s ] -> Assert.Equal("acc", s.AccumulatorName)
    | other -> failwithf "Expected exactly one List.append note, got %A" other

[<Fact>]
let ``cons is the fix, not a finding`` () =
    Assert.Empty(
        recursiveAppendsIn
            "module Test\nlet rec collect acc xs =\n    match xs with\n    | [] -> List.rev acc\n    | x :: rest -> collect (x :: acc) rest"
    )

[<Fact>]
let ``a general merge may be exactly what the author wants`` () =
    Assert.Empty(
        recursiveAppendsIn
            "module Test\nlet rec collect acc xs =\n    match xs with\n    | [] -> acc\n    | x :: rest -> collect (acc @ expand x) rest\nand expand (x: int) : int list = [ x; x ]"
    )

[<Fact>]
let ``a nested loop rebinding the index keeps the outer loop`` () =
    // the inner `i` shadows: rewriting `xs.[i]` to the OUTER element would
    // silently change behavior
    Assert.Empty(
        indexedLoopsIn
            "module Test\nlet f (xs: int[]) =\n    for i in 0 .. xs.Length - 1 do\n        for i in 0 .. 2 do\n            printfn \"%d\" xs.[i]"
    )

[<Fact>]
let ``a match pattern rebinding the index keeps the loop`` () =
    Assert.Empty(
        indexedLoopsIn
            "module Test\nlet f (xs: int[]) (q: int) =\n    for i in 0 .. xs.Length - 1 do\n        match q with\n        | i -> printfn \"%d\" xs.[i]"
    )

[<Fact>]
let ``an index used as a value inside an F#6 indexer-set is seen`` () =
    // from Fuuga's EvalTests: the SDK walker skips BOTH sides of
    // `logits[...] <- v` (SynExpr.Set), so `int64 pos` was invisible and
    // the loop got rewritten with `pos` still referenced. The AstIndex
    // graft now lifts Set's children.
    Assert.Empty(
        indexedLoopsIn
            "module Test\nlet f (tokens: int[]) (logits: int64[,,]) =\n    for pos in 0 .. tokens.Length - 1 do\n        let nextToken = min 31 (tokens.[pos] + 1)\n        logits[0L, int64 pos, int64 nextToken] <- 100L"
    )

[<Fact>]
let ``an F#6 element write needs the index too`` () =
    Assert.Empty(
        indexedLoopsIn
            "module Test\nlet f (xs: int[]) =\n    for i in 0 .. xs.Length - 1 do\n        xs[i] <- xs[i] + 1"
    )
