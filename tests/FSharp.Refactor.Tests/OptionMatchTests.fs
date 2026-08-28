module FSharp.Refactor.Tests.OptionMatchTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private optionMatchIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    OptionMatch.find tree sourceText checkResults

let private assertOptionMatch (source: string) (expectedReplacement: string) =
    match optionMatchIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one option-match suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``IsSome with bare Value becomes a match`` () =
    assertOptionMatch "let f (x: int option) = if x.IsSome then x.Value else 0" "match x with | Some v -> v | None -> 0"

[<Fact>]
let ``IsNone form swaps the branches`` () =
    assertOptionMatch "let f (x: int option) = if x.IsNone then 0 else x.Value" "match x with | Some v -> v | None -> 0"

[<Fact>]
let ``negated IsSome is the IsNone form`` () =
    assertOptionMatch
        "let f (x: int option) = if not x.IsSome then 0 else x.Value"
        "match x with | Some v -> v | None -> 0"

[<Fact>]
let ``value option spells ValueSome`` () =
    assertOptionMatch
        "let f (x: int voption) = if x.IsSome then x.Value else 0"
        "match x with | ValueSome v -> v | ValueNone -> 0"

[<Fact>]
let ``Value inside a larger expression is substituted`` () =
    assertOptionMatch
        "let f (x: int option) = if x.IsSome then x.Value + 1 else 0"
        "match x with | Some v -> v + 1 | None -> 0"

[<Fact>]
let ``Value prefix of a longer path is substituted`` () =
    assertOptionMatch
        "let f (x: string option) = if x.IsSome then x.Value.Length else 0"
        "match x with | Some v -> v.Length | None -> 0"

[<Fact>]
let ``else-less unit conditional gains a unit clause`` () =
    assertOptionMatch
        "let f (x: int option) = if x.IsSome then printfn \"%d\" x.Value"
        "match x with | Some v -> printfn \"%d\" v | None -> ()"

[<Fact>]
let ``binder falls back when v is taken`` () =
    assertOptionMatch
        "let f (v: int) (x: int option) = if x.IsSome then x.Value + v else v"
        "match x with | Some xValue -> xValue + v | None -> v"

[<Fact>]
let ``Value in the None arm is left alone`` () =
    Assert.Empty(optionMatchIn "let f (x: int option) = if x.IsSome then 1 else x.Value")

[<Fact>]
let ``no Value use is left alone`` () =
    Assert.Empty(optionMatchIn "let f (x: int option) = if x.IsSome then 1 else 2")

[<Fact>]
let ``custom type with IsSome and Value members is left alone`` () =
    Assert.Empty(
        optionMatchIn
            "type Box(v: int) =\n    member _.IsSome = true\n    member _.Value = v\nlet f (x: Box) = if x.IsSome then x.Value else 0"
    )

[<Fact>]
let ``IsSome and a Value predicate becomes Option exists`` () =
    assertOptionMatch "let f (x: int option) = x.IsSome && x.Value > 3" "x |> Option.exists (fun v -> v > 3)"

[<Fact>]
let ``an and-chain of predicates joins inside the lambda`` () =
    assertOptionMatch
        "let f (x: int option) = x.IsSome && x.Value > 3 && x.Value < 10"
        "x |> Option.exists (fun v -> v > 3 && v < 10)"

[<Fact>]
let ``IsNone or a Value predicate becomes Option forall`` () =
    assertOptionMatch "let f (x: int option) = x.IsNone || x.Value > 3" "x |> Option.forall (fun v -> v > 3)"

[<Fact>]
let ``a combo without any Value use stays`` () =
    // `x.IsSome && flag` gains nothing from a lambda
    Assert.Empty(optionMatchIn "let f (x: int option) (flag: bool) = x.IsSome && flag")

[<Fact>]
let ``IsNone with && is not the exists shape`` () =
    Assert.Empty(optionMatchIn "let g (x: int option) = x.IsNone && (try x.Value > 3 with _ -> false)")

[<Fact>]
let ``a voption combo uses the ValueOption module`` () =
    assertOptionMatch "let f (x: int voption) = x.IsSome && x.Value > 3" "x |> ValueOption.exists (fun v -> v > 3)"

[<Fact>]
let ``a multiline if with single-line branches now rewrites`` () =
    assertOptionMatch
        "let f (x: int option) =\n    if x.IsSome then\n        x.Value + 1\n    else\n        0"
        "match x with | Some v -> v + 1 | None -> 0"
