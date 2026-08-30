module FSharp.Refactor.Tests.VectorizedLinqTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private vectorizedIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    VectorizedLinq.find tree sourceText checkResults

[<Fact>]
let ``Array sum on an int array is noted`` () =
    match vectorizedIn "let f (values: int[]) = Array.sum values" with
    | [ s ] ->
        Assert.Equal("sum", s.FunctionName)
        Assert.Equal("values", s.ArrayName)
    | other -> failwithf "Expected exactly one vectorization note, got %A" other

[<Fact>]
let ``piped Array max on an int64 array is noted`` () =
    match vectorizedIn "let f (values: int64[]) = values |> Array.max" with
    | [ s ] -> Assert.Equal("max", s.FunctionName)
    | other -> failwithf "Expected exactly one piped note, got %A" other

[<Fact>]
let ``Seq sum over an int array is the same scalar loop`` () =
    match vectorizedIn "let f (values: int[]) = values |> Seq.sum" with
    | [ s ] ->
        Assert.Equal("Seq", s.ModuleName)
        Assert.Equal("sum", s.FunctionName)
    | other -> failwithf "Expected exactly one Seq-over-array note, got %A" other

[<Fact>]
let ``Seq sum over a list has no vectorized sibling`` () =
    Assert.Empty(vectorizedIn "let f (values: int list) = values |> Seq.sum")

[<Fact>]
let ``float arrays are excluded for NaN semantics`` () =
    Assert.Empty(vectorizedIn "let f (values: float[]) = Array.sum values")

[<Fact>]
let ``lists are not arrays`` () =
    Assert.Empty(vectorizedIn "let f (values: int list) = List.sum values")

[<Fact>]
let ``Array sum of a non-primitive is left alone`` () =
    Assert.Empty(vectorizedIn "let f (values: decimal[]) = Array.sum values")

[<Fact>]
let ``a record-field array aggregation is noted`` () =
    // the field resolves as FSharpField, not a member-or-value
    let suggestions =
        vectorizedIn "type State = { Buffer: int[] }\nlet f (state: State) = state.Buffer |> Array.sum"

    match suggestions with
    | [ s ] -> Assert.Equal("state.Buffer", s.ArrayName)
    | other -> failwithf "Expected exactly one vectorized note, got %A" other

[<Fact>]
let ``piped Array contains on an int array is noted`` () =
    match vectorizedIn "let f (values: int[]) = values |> Array.contains 42" with
    | [ s ] ->
        Assert.Equal("contains", s.FunctionName)
        Assert.Equal("values", s.ArrayName)
    | other -> failwithf "Expected exactly one contains note, got %A" other

[<Fact>]
let ``direct Array contains on an int array is noted`` () =
    match vectorizedIn "let f (values: int[]) = Array.contains 42 values" with
    | [ s ] -> Assert.Equal("contains", s.FunctionName)
    | other -> failwithf "Expected exactly one direct contains note, got %A" other

[<Fact>]
let ``Array contains on a list stays quiet`` () =
    Assert.Empty(vectorizedIn "let f (values: int list) = values |> List.contains 42")

[<Fact>]
let ``Array contains on a float array stays quiet`` () =
    // NaN: Enumerable.Contains uses EqualityComparer, F# contains uses (=)
    Assert.Empty(vectorizedIn "let f (values: float[]) = values |> Array.contains 4.2")

[<Fact>]
let ``a contains inside a query expression stays quiet`` () =
    // inside query { } this is a quotation for a provider's translator:
    // SQLProvider turns Array.contains into SQL IN, and the Enumerable
    // spelling may not translate at all
    Assert.Empty(
        vectorizedIn
            "let f (values: int[]) (xs: int list) =\n    query {\n        for x in xs do\n            where (values |> Array.contains x)\n            select x\n    }"
    )

[<Fact>]
let ``a sum inside a query expression stays quiet too`` () =
    Assert.Empty(
        vectorizedIn
            "let f (values: int[]) (xs: int list) =\n    query {\n        for x in xs do\n            select (Array.sum values + x)\n    }"
    )
