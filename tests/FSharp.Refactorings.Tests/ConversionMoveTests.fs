module FSharp.Refactorings.Tests.ConversionMoveTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

let private findIn (source: string) =
    let tree, sourceText = parse source
    ConversionMove.find tree sourceText

/// Expect one suggestion; verify the fully patched source text and that it
/// still parses.
let private assertPatched (source: string) (expectedPatched: string) =
    match findIn source with
    | [ s ] ->
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.Equal(expectedPatched, patched)
        Assert.True(parsesCleanly patched, sprintf "Patched source does not parse:\n%s" patched)
    | other -> failwithf "Expected exactly one suggestion, got %d: %A" (List.length other) other

let private assertNoSuggestion (source: string) = Assert.Empty(findIn source)

[<Fact>]
let ``seq-to-list conversion moves past map`` () =
    assertPatched
        "module Test\nlet f g xs = xs |> Seq.toList |> List.map g"
        "module Test\nlet f g xs = xs |> Seq.map g |> Seq.toList"

[<Fact>]
let ``ofSeq spelling is preserved when moved`` () =
    assertPatched
        "module Test\nlet f g xs = xs |> List.ofSeq |> List.filter g"
        "module Test\nlet f g xs = xs |> Seq.filter g |> List.ofSeq"

[<Fact>]
let ``seq-to-array conversion moves past map`` () =
    assertPatched
        "module Test\nlet f g xs = xs |> Seq.toArray |> Array.map g"
        "module Test\nlet f g xs = xs |> Seq.map g |> Seq.toArray"

[<Fact>]
let ``list-to-array conversion moves past choose`` () =
    assertPatched
        "module Test\nlet f g xs = xs |> List.toArray |> Array.choose g"
        "module Test\nlet f g xs = xs |> List.choose g |> List.toArray"

[<Fact>]
let ``array-to-list conversion moves past map`` () =
    assertPatched
        "module Test\nlet f g xs = xs |> Array.toList |> List.map g"
        "module Test\nlet f g xs = xs |> Array.map g |> Array.toList"

[<Fact>]
let ``conversion before length is dropped`` () =
    assertPatched "module Test\nlet f xs = xs |> Seq.toList |> List.length" "module Test\nlet f xs = xs |> Seq.length"

[<Fact>]
let ``conversion before iter is dropped`` () =
    assertPatched
        "module Test\nlet f g xs = xs |> Seq.toList |> List.iter g"
        "module Test\nlet f g xs = xs |> Seq.iter g"

[<Fact>]
let ``mid-pipeline segment is rewritten in place`` () =
    assertPatched
        "module Test\nlet f g h k xs = xs |> h |> Seq.toList |> List.map g |> k"
        "module Test\nlet f g h k xs = xs |> h |> Seq.map g |> Seq.toList |> k"

[<Fact>]
let ``multi-line pipeline is rewritten and collapses two stages`` () =
    assertPatched
        "module Test\nlet f g xs =\n    xs\n    |> Seq.toList\n    |> List.map g"
        "module Test\nlet f g xs =\n    xs\n    |> Seq.map g |> Seq.toList"

[<Fact>]
let ``lambda argument text is preserved verbatim`` () =
    assertPatched
        "module Test\nlet f xs = xs |> Seq.toList |> List.map (fun v -> v + 1)"
        "module Test\nlet f xs = xs |> Seq.map (fun v -> v + 1) |> Seq.toList"

[<Fact>]
let ``operation from a different module is not rewritten`` () =
    assertNoSuggestion "module Test\nlet f g xs = xs |> Seq.toList |> Array.map g"

[<Fact>]
let ``non-whitelisted operation is not rewritten`` () =
    // List.skip and Seq.skip throw different exception types on short input
    assertNoSuggestion "module Test\nlet f xs = xs |> Seq.toList |> List.skip 1"

[<Fact>]
let ``groupBy is not rewritten`` () =
    // Seq.groupBy yields seq-valued groups: the element type would change
    assertNoSuggestion "module Test\nlet f (g: int -> int) xs = xs |> Seq.toList |> List.groupBy g"

[<Fact>]
let ``sortBy conversion moves`` () =
    assertPatched
        "module Test\nlet f (g: int -> int) xs = xs |> Seq.toList |> List.sortBy g"
        "module Test\nlet f (g: int -> int) xs = xs |> Seq.sortBy g |> Seq.toList"

[<Fact>]
let ``rev conversion moves`` () =
    assertPatched
        "module Test\nlet f xs = xs |> Array.toList |> List.rev"
        "module Test\nlet f xs = xs |> Array.rev |> Array.toList"

[<Fact>]
let ``conversion before exists is dropped`` () =
    assertPatched
        "module Test\nlet f (p: int -> bool) xs = xs |> Seq.toList |> List.exists p"
        "module Test\nlet f (p: int -> bool) xs = xs |> Seq.exists p"

[<Fact>]
let ``conversion before isEmpty is dropped`` () =
    assertPatched "module Test\nlet f xs = xs |> Seq.toList |> List.isEmpty" "module Test\nlet f xs = xs |> Seq.isEmpty"

[<Fact>]
let ``conversion before fold is dropped`` () =
    assertPatched
        "module Test\nlet f xs = xs |> Seq.toList |> List.fold (+) 0"
        "module Test\nlet f xs = xs |> Seq.fold (+) 0"

[<Fact>]
let ``conversion toward seq is never moved`` () =
    // rewriting would turn eager code lazy
    assertNoSuggestion "module Test\nlet f g xs = xs |> Seq.ofList |> Seq.map g"

[<Fact>]
let ``pipeline without conversion is not rewritten`` () =
    assertNoSuggestion "module Test\nlet f g xs = xs |> List.map g |> List.filter g"

[<Fact>]
let ``collect is not moved across a List-Array boundary`` () =
    // review regression: List.collect needs a list-returning mapper
    assertNoSuggestion "module Test\nlet f (g: int -> int[]) xs = xs |> List.toArray |> Array.collect g"

[<Fact>]
let ``collect moves for Seq-sourced conversions`` () =
    assertPatched
        "module Test\nlet f (g: int -> int list) xs = xs |> Seq.toList |> List.collect g"
        "module Test\nlet f (g: int -> int list) xs = xs |> Seq.collect g |> Seq.toList"

[<Fact>]
let ``sort family is not moved across an Array boundary`` () =
    // review regression: Array sorts are unstable, Seq/List sorts are stable
    assertNoSuggestion "module Test\nlet f (g: int -> int) xs = xs |> Seq.toArray |> Array.sortBy g"

[<Fact>]
let ``item is not treated as consuming`` () =
    // review regression: Array.item and List.item throw different exception types
    assertNoSuggestion "module Test\nlet f xs = xs |> Seq.toList |> List.item 1"
