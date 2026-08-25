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
let ``float arrays are excluded for NaN semantics`` () =
    Assert.Empty(vectorizedIn "let f (values: float[]) = Array.sum values")

[<Fact>]
let ``lists are not arrays`` () =
    Assert.Empty(vectorizedIn "let f (values: int list) = List.sum values")

[<Fact>]
let ``Array sum of a non-primitive is left alone`` () =
    Assert.Empty(vectorizedIn "let f (values: decimal[]) = Array.sum values")
