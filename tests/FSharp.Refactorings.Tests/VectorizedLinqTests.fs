module FSharp.Refactorings.Tests.VectorizedLinqTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

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
