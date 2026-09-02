module FSharp.Refactor.Tests.GenerativeLoopTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

// ---- FR0141 GenerativeLoop ----

let private genLoopIn (source: string) =
    let tree, sourceText = parse source
    GenerativeLoop.find tree sourceText

[<Fact>]
let ``state carried forward through a flag loop is noted`` () =
    // the VisionInference shape: cache feeds the next round
    let source =
        "module Test\nlet f (model: int) (limit: int) =\n    let generated = ResizeArray<int>()\n    let mutable cache = 0\n    let mutable stopped = false\n    while not stopped && generated.Count < limit do\n        let next = model + cache\n        cache <- next\n        if next = 0 then stopped <- true\n        else generated.Add next\n    generated"

    match genLoopIn source with
    | [ s ] ->
        Assert.Equal("stopped", s.Flag)
        Assert.Equal<string list>([ "cache" ], s.Carried)
    | other -> failwithf "Expected exactly one note, got %A" other

[<Fact>]
let ``a search loop carrying only an index is left alone`` () =
    // already short-circuits, allocates nothing, and measured 12x faster
    // than Array.exists on an early hit — a pipeline would be a regression
    let source =
        "module Test\nlet f (xs: int[]) =\n    let mutable found = false\n    let mutable i = 0\n    while not found && i < xs.Length do\n        if xs.[i] > 2 then found <- true\n        i <- i + 1\n    found"

    Assert.Empty(genLoopIn source)

[<Fact>]
let ``a decrementing index is still just an index`` () =
    let source =
        "module Test\nlet f (xs: int[]) =\n    let mutable found = false\n    let mutable i = xs.Length - 1\n    while not found && i >= 0 do\n        if xs.[i] > 2 then found <- true\n        i <- i - 1\n    found"

    Assert.Empty(genLoopIn source)

[<Fact>]
let ``a mutable declared INSIDE the body is not carried`` () =
    let source =
        "module Test\nlet f (n: int) =\n    let mutable stopped = false\n    let mutable i = 0\n    while not stopped && i < n do\n        let mutable scratch = 0\n        scratch <- i * 2\n        if scratch > 10 then stopped <- true\n        i <- i + 1\n    stopped"

    Assert.Empty(genLoopIn source)

[<Fact>]
let ``a loop whose flag is never raised is waiting on something else`` () =
    let source =
        "module Test\nlet f (ready: bool) (n: int) =\n    let mutable acc = 0\n    let mutable i = 0\n    while not ready && i < n do\n        acc <- acc + i\n        i <- i + 1\n    acc"

    Assert.Empty(genLoopIn source)

[<Fact>]
let ``the tail after the flag is counted`` () =
    // two statements still run in the iteration that raised the flag
    let source =
        "module Test\nlet f (n: int) =\n    let acc = ResizeArray<int>()\n    let mutable cache = 0\n    let mutable stopped = false\n    let mutable i = 0\n    while not stopped && i < n do\n        let next = cache + i\n        if next > 10 then stopped <- true\n        cache <- next\n        i <- i + 1\n    acc"

    match genLoopIn source with
    | [ s ] ->
        Assert.Equal(2, s.TailAfterFlag)
        Assert.Contains("cache", s.Carried)
    | other -> failwithf "Expected exactly one note, got %A" other

[<Fact>]
let ``a flag raised last has no tail to claim`` () =
    let source =
        "module Test\nlet f (n: int) =\n    let acc = ResizeArray<int>()\n    let mutable cache = 0\n    let mutable stopped = false\n    while not stopped && acc.Count < n do\n        let next = cache + 1\n        cache <- next\n        if next > 10 then stopped <- true\n        else acc.Add next\n    acc"

    match genLoopIn source with
    | [ s ] -> Assert.Equal(0, s.TailAfterFlag)
    | other -> failwithf "Expected exactly one note, got %A" other

[<Fact>]
let ``a loop inside a task CE is left alone - no tail calls there`` () =
    // FSharp.Azure.Quantum's polling loop carries this reason in a comment
    // above it: a task compiles to a state machine and recursive return!
    // grows the stack, so the advice would be wrong
    let source =
        "module Test\nopen System.Threading.Tasks\nlet f (n: int) =\n    task {\n        let mutable cache = 0\n        let mutable finished = false\n        while not finished do\n            let next = cache + 1\n            cache <- next\n            if next > n then finished <- true\n        return cache\n    }"

    Assert.Empty(genLoopIn source)

[<Fact>]
let ``a loop inside an async CE is left alone too`` () =
    // the recursive function would have to return Async<_>, so the rewrite
    // reaches the signature rather than the loop
    let source =
        "module Test\nlet f (n: int) =\n    async {\n        let mutable cache = 0\n        let mutable finished = false\n        while not finished do\n            let next = cache + 1\n            cache <- next\n            if next > n then finished <- true\n        return cache\n    }"

    Assert.Empty(genLoopIn source)

[<Fact>]
let ``a loop inside a seq CE is ordinary code and still gets the advice`` () =
    // only the asynchronous builders are excluded
    let source =
        "module Test\nlet f (n: int) =\n    seq {\n        let mutable cache = 0\n        let mutable finished = false\n        while not finished do\n            let next = cache + 1\n            cache <- next\n            if next > n then finished <- true\n        yield cache\n    }"

    match genLoopIn source with
    | [ s ] -> Assert.Equal("finished", s.Flag)
    | other -> failwithf "Expected exactly one note, got %A" other
