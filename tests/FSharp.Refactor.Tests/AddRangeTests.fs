module FSharp.Refactor.Tests.AddRangeTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private addRangeIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    AddRange.find tree sourceText checkResults

let private assertAddRange (source: string) (expectedReplacement: string) =
    match addRangeIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one AddRange suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``plain element accumulation becomes AddRange`` () =
    assertAddRange
        "let f (acc: ResizeArray<int>) (xs: int list) =\n    for x in xs do\n        acc.Add x"
        "acc.AddRange xs"

[<Fact>]
let ``projected accumulation becomes AddRange over Seq map`` () =
    assertAddRange
        "let f (acc: ResizeArray<int>) (xs: int list) =\n    for x in xs do\n        acc.Add(x * 2)"
        "acc.AddRange(xs |> Seq.map (fun x -> x * 2))"

[<Fact>]
let ``tuple loop pattern is parenthesized in the lambda`` () =
    assertAddRange
        "let f (acc: ResizeArray<int>) (ps: (int * int) list) =\n    for (a, b) in ps do\n        acc.Add(a + b)"
        "acc.AddRange(ps |> Seq.map (fun (a, b) -> a + b))"

[<Fact>]
let ``property receiver keeps its path`` () =
    assertAddRange
        "type Holder() =\n    member val Items = ResizeArray<int>() with get\nlet f (h: Holder) (xs: int list) =\n    for x in xs do\n        h.Items.Add x"
        "h.Items.AddRange xs"

[<Fact>]
let ``extra statements in the body are left alone`` () =
    Assert.Empty(
        addRangeIn
            "let f (acc: ResizeArray<int>) (xs: int list) =\n    for x in xs do\n        printfn \"%d\" x\n        acc.Add x"
    )

[<Fact>]
let ``HashSet Add is a different beast`` () =
    Assert.Empty(
        addRangeIn
            "let f (acc: System.Collections.Generic.HashSet<int>) (xs: int list) =\n    for x in xs do\n        acc.Add x |> ignore"
    )

[<Fact>]
let ``loop over an expression source is parenthesized`` () =
    assertAddRange
        "let f (acc: ResizeArray<int>) (xs: int list) =\n    for x in List.rev xs do\n        acc.Add x"
        "acc.AddRange (List.rev xs)"
