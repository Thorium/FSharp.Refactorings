module FSharp.Refactorings.Tests.MutableRemovalTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

let private findIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    MutableRemoval.find tree sourceText checkResults

let private assertSingleSuggestion (source: string) (expectedPatched: string) =
    match findIn source with
    | [ s ] ->
        let patched = applyEdit source s.Range ""
        Assert.Equal(expectedPatched, patched)
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one suggestion, got %d: %A" (List.length other) other

let private assertNoSuggestion (source: string) = Assert.Empty(findIn source)

[<Fact>]
let ``never-assigned mutable int is flagged`` () =
    assertSingleSuggestion "let f () =\n    let mutable x = 0\n    x + 1" "let f () =\n    let x = 0\n    x + 1"

[<Fact>]
let ``never-assigned mutable string is flagged`` () =
    assertSingleSuggestion
        "let f (s: string) =\n    let mutable name = s\n    name.Length"
        "let f (s: string) =\n    let name = s\n    name.Length"

[<Fact>]
let ``whitelisted struct is flagged`` () =
    assertSingleSuggestion
        "let f () =\n    let mutable g = System.Guid.NewGuid()\n    g.ToString()"
        "let f () =\n    let g = System.Guid.NewGuid()\n    g.ToString()"

[<Fact>]
let ``assigned binding is not flagged`` () =
    assertNoSuggestion "let f () =\n    let mutable x = 0\n    x <- 1\n    x"

[<Fact>]
let ``assignment inside a closure is not flagged`` () =
    assertNoSuggestion "let f () =\n    let mutable x = 0\n    let bump () = x <- x + 1\n    bump ()\n    x"

[<Fact>]
let ``address-of use is not flagged`` () =
    assertNoSuggestion
        "let f () =\n    let mutable x = 0\n    System.Threading.Interlocked.Increment(&x) |> ignore\n    x"

[<Fact>]
let ``property assignment through the binding is not flagged`` () =
    assertNoSuggestion
        "type C() = member val P = 0 with get, set\nlet f () =\n    let mutable c = C()\n    c.P <- 1\n    c"

[<Fact>]
let ``non-whitelisted struct is not flagged`` () =
    // removing mutable would introduce defensive copies for member calls
    assertNoSuggestion "[<Struct>] type S = { A: int }\nlet f () =\n    let mutable s = { A = 1 }\n    s.A"

[<Fact>]
let ``module-level mutable is not flagged`` () =
    assertNoSuggestion "module M\nlet mutable counter = 0\nlet read () = counter"
