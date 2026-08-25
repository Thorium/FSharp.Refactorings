module FSharp.Refactorings.Tests.CompositionTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

let private findIn (source: string) =
    let tree, sourceText = parse source
    Composition.find tree sourceText

let private assertSingleSuggestion (source: string) (expectedReplacement: string) =
    match findIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(parsesCleanly patched, sprintf "Patched source does not parse:\n%s" patched)
    | other -> failwithf "Expected exactly one suggestion, got %d: %A" (List.length other) other

let private assertNoSuggestion (source: string) = Assert.Empty(findIn source)

[<Fact>]
let ``pipeline lambda becomes composition`` () =
    assertSingleSuggestion "module Test\nlet f g h xs = xs |> List.map (fun x -> x |> g |> h)" "g >> h"

[<Fact>]
let ``nested application lambda becomes composition`` () =
    assertSingleSuggestion "module Test\nlet f g h xs = xs |> List.map (fun x -> h (g x))" "g >> h"

[<Fact>]
let ``three-stage pipeline`` () =
    assertSingleSuggestion "module Test\nlet f g h k xs = xs |> List.map (fun x -> x |> g |> h |> k)" "g >> h >> k"

[<Fact>]
let ``partial application stages`` () =
    assertSingleSuggestion
        "module Test\nlet f g h xs = xs |> List.map (fun x -> x |> List.map g |> List.filter h)"
        "List.map g >> List.filter h"

[<Fact>]
let ``nested application with partial application`` () =
    assertSingleSuggestion "module Test\nlet f g h xs = xs |> List.map (fun x -> List.map g (h x))" "h >> List.map g"

[<Fact>]
let ``operator-section stage stays bare`` () =
    assertSingleSuggestion "module Test\nlet f g xs = xs |> List.map (fun x -> x |> g |> (+) 1)" "g >> (+) 1"

[<Fact>]
let ``stage referencing the parameter is not rewritten`` () =
    assertNoSuggestion "module Test\nlet f g xs = xs |> List.map (fun x -> x |> g |> List.append x)"

[<Fact>]
let ``single stage is not rewritten`` () =
    // eta-reduction territory, not composition
    assertNoSuggestion "module Test\nlet f g xs = xs |> List.map (fun x -> g x)"

[<Fact>]
let ``let-bound lambda is not rewritten`` () =
    // rewriting `let h = fun x -> ...` risks the value restriction
    assertNoSuggestion "module Test\nlet h = fun x -> x |> List.map id |> List.length"

[<Fact>]
let ``two parameters are not rewritten`` () =
    assertNoSuggestion "module Test\nlet f g h xs = xs |> List.mapi (fun i x -> x |> g |> h i)"

[<Fact>]
let ``annotated parameter is not rewritten`` () =
    // the annotation would be lost in the rewrite
    assertNoSuggestion "module Test\nlet f g h xs = xs |> List.map (fun (x: int) -> x |> g |> h)"

[<Fact>]
let ``pipeline not starting from the parameter is not rewritten`` () =
    assertNoSuggestion "module Test\nlet f g h y xs = xs |> List.map (fun x -> y |> g |> h)"

[<Fact>]
let ``infix body is not decomposed into an invalid stage`` () =
    // review regression: `(1 +) >> g` is not valid F#
    assertNoSuggestion "module Test\nlet f g xs = xs |> List.map (fun x -> g (1 + x))"

[<Fact>]
let ``parenthesized let-bound lambda is not rewritten`` () =
    // review regression: the composition form falls under the value restriction
    assertNoSuggestion "module Test\nlet h = (fun x -> x |> Seq.map id |> Seq.toList)"
