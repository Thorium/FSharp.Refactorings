module FSharp.Refactor.Tests.CeStripTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private findIn (source: string) =
    let tree, sourceText = parse source
    CeStrip.find tree sourceText

let private assertSingleSuggestion (source: string) (expectedReplacement: string) =
    match findIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(parsesCleanly patched, sprintf "Patched source does not parse:\n%s" patched)
    | other -> failwithf "Expected exactly one suggestion, got %d: %A" (List.length other) other

let private assertNoSuggestion (source: string) = Assert.Empty(findIn source)

[<Fact>]
let ``return-bang of identifier is stripped`` () =
    assertSingleSuggestion "module Test\nlet f (comp: Async<int>) = async { return! comp }" "comp"

[<Fact>]
let ``let-bang rewrap identity is stripped`` () =
    assertSingleSuggestion "module Test\nlet f (comp: Async<int>) = async { let! v = comp in return v }" "comp"

[<Fact>]
let ``multi-line let-bang rewrap identity is stripped`` () =
    assertSingleSuggestion
        "module Test\nlet f (comp: Async<int>) =\n    async {\n        let! v = comp\n        return v\n    }"
        "comp"

[<Fact>]
let ``wrap piped to RunSynchronously is stripped and parenthesized`` () =
    assertSingleSuggestion "module Test\nlet f x = async { return x + 1 } |> Async.RunSynchronously" "(x + 1)"

[<Fact>]
let ``wrap applied to RunSynchronously is stripped and parenthesized`` () =
    assertSingleSuggestion "module Test\nlet f x = Async.RunSynchronously (async { return x + 1 })" "(x + 1)"

[<Fact>]
let ``atomic returned value stays unparenthesized`` () =
    assertSingleSuggestion "module Test\nlet f x = async { return x } |> Async.RunSynchronously" "x"

[<Fact>]
let ``tuple return inside a tuple context keeps its grouping`` () =
    // review regression: `(1, 2, "tag")` would silently flatten the pair
    assertSingleSuggestion "module Test\nlet pair = (async { return 1, 2 } |> Async.RunSynchronously, \"tag\")" "(1, 2)"

[<Fact>]
let ``runner strip as an operand keeps precedence`` () =
    // review regression: bare `a + b * 2` would compute a + (b*2)
    assertSingleSuggestion "module Test\nlet f a b = Async.RunSynchronously (async { return a + b }) * 2" "(a + b)"

[<Fact>]
let ``dotted path computation is not stripped`` () =
    // a dotted path can be a property getter; stripping would move its
    // evaluation from per-run to construction time
    assertNoSuggestion "module Test\ntype S = { Comp: Async<int> }\nlet f (s: S) = async { return! s.Comp }"

[<Fact>]
let ``plain return without runner is not stripped`` () =
    // stripping would change the type from Async<'T> to 'T
    assertNoSuggestion "module Test\nlet f x = async { return x + 1 }"

[<Fact>]
let ``return-bang of application is not stripped`` () =
    // evaluating `g x` early could run construction-time side effects
    assertNoSuggestion "module Test\nlet f g x = async { return! g x }"

[<Fact>]
let ``use-bang is never stripped`` () =
    assertNoSuggestion "module Test\nlet f (comp: Async<System.IDisposable>) = async { use! v = comp in return v }"

[<Fact>]
let ``let-bang with different returned value is not stripped`` () =
    assertNoSuggestion "module Test\nlet f (comp: Async<int>) (w: int) = async { let! v = comp in return w }"

[<Fact>]
let ``let-bang with transformed result is not stripped`` () =
    assertNoSuggestion "module Test\nlet f (comp: Async<int>) = async { let! v = comp in return v + 1 }"

[<Fact>]
let ``binding computation is not stripped`` () =
    assertNoSuggestion "module Test\nlet f (a: Async<int>) (g: int -> Async<int>) = async { let! v = a in return! g v }"

[<Fact>]
let ``task return-bang is not touched`` () =
    // task's return! also accepts Async<'T>; stripping could change the type
    assertNoSuggestion "module Test\nlet f (comp: System.Threading.Tasks.Task<int>) = task { return! comp }"

[<Fact>]
let ``task wrapping a constant becomes Task-FromResult`` () =
    assertSingleSuggestion "module Test\nopen System.Threading.Tasks\nlet f () = task { return 3 }" "Task.FromResult(3)"

[<Fact>]
let ``task wrapping an identifier becomes Task-FromResult`` () =
    assertSingleSuggestion "module Test\nopen System.Threading.Tasks\nlet f x = task { return x }" "Task.FromResult(x)"

[<Fact>]
let ``task wrapping without the open is not rewritten`` () =
    // Task.FromResult would not resolve
    assertNoSuggestion "module Test\nlet f x = task { return x }"

[<Fact>]
let ``task wrapping an expression that could throw is not rewritten`` () =
    // task { return e } yields a faulted task on throw; Task.FromResult e throws synchronously
    assertNoSuggestion "module Test\nopen System.Threading.Tasks\nlet f (x: int) = task { return x + 1 }"
