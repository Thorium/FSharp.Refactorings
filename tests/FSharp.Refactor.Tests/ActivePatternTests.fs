module FSharp.Refactor.Tests.ActivePatternTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private findIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    ActivePattern.find tree sourceText checkResults

/// Apply both edits of the suggestion (clause first — it sits later in the
/// document — then the insertion) and verify the result typechecks.
let private assertSingleSuggestion (source: string) (expectedPatched: string) =
    match findIn source with
    | [ s ] ->
        let patched = applyEdit source s.ClauseRange s.ClauseText
        let patched = applyEdit patched s.InsertRange s.InsertText
        Assert.Equal(expectedPatched, patched)
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one suggestion, got %d: %A" (List.length other) other

let private assertNoSuggestion (source: string) = Assert.Empty(findIn source)

[<Fact>]
let ``dotted guard function becomes an active pattern`` () =
    assertSingleSuggestion
        "module Test\nlet describe (s: string) =\n    match s with\n    | s when System.String.IsNullOrEmpty s -> \"empty\"\n    | s -> s"
        // a .NET member's extracted input is annotated with its resolved
        // parameter type — for the overloaded ones (Path.IsPathRooted) it
        // is the difference between compiling and FS0041
        "module Test\n[<return: Struct>]\nlet inline private (|IsNullOrEmpty|_|) (input: string) =\n    if System.String.IsNullOrEmpty input then ValueSome input else ValueNone\nlet describe (s: string) =\n    match s with\n    | IsNullOrEmpty s -> \"empty\"\n    | s -> s"

[<Fact>]
let ``module-level guard function becomes an active pattern`` () =
    assertSingleSuggestion
        "module Test\nlet isEven (n: int) = n % 2 = 0\nlet f x =\n    match x with\n    | n when isEven n -> n\n    | n -> 0"
        "module Test\nlet isEven (n: int) = n % 2 = 0\n[<return: Struct>]\nlet inline private (|IsEven|_|) input =\n    if isEven input then ValueSome input else ValueNone\nlet f x =\n    match x with\n    | IsEven n -> n\n    | n -> 0"

[<Fact>]
let ``locally defined guard function is not extracted`` () =
    // the generated binding would sit outside isOdd's scope
    assertNoSuggestion
        "module Test\nlet f x =\n    let isOdd (n: int) = n % 2 = 1\n    match x with\n    | n when isOdd n -> n\n    | _ -> 0"

[<Fact>]
let ``guard using a lambda parameter function is not extracted`` () =
    assertNoSuggestion
        "module Test\nlet f (check: int -> bool) x =\n    match x with\n    | n when check n -> n\n    | _ -> 0"

[<Fact>]
let ``existing pattern of the same name suppresses the hint`` () =
    assertNoSuggestion
        "module Test\nlet isEven (n: int) = n % 2 = 0\nlet (|IsEven|_|) (n: int) = if isEven n then Some n else None\nlet f x =\n    match x with\n    | n when isEven n -> n\n    | _ -> 0"

[<Fact>]
let ``complex guard expression is not extracted`` () =
    assertNoSuggestion "module Test\nlet f x =\n    match x with\n    | n when n % 2 = 0 -> n\n    | _ -> 0"

[<Fact>]
let ``guard applied to a different value is not extracted`` () =
    assertNoSuggestion
        "module Test\nlet isEven (n: int) = n % 2 = 0\nlet f x y =\n    match x with\n    | n when isEven y -> n\n    | _ -> 0"

[<Fact>]
let ``guard inside a member is not extracted`` () =
    // review regression: the inserted binding would sit before the type, where
    // the member parameter is out of scope
    assertNoSuggestion
        "module Test\ntype T() =\n    member _.Check f x =\n        match x with\n        | n when f n -> 1\n        | _ -> 0"

[<Fact>]
let ``repeated guards yield a single suggestion`` () =
    // review regression: applying two identical insertions would produce a
    // duplicate definition
    let suggestions =
        findIn
            "module Test\nlet isEven (n: int) = n % 2 = 0\nlet f x =\n    match x with\n    | n when isEven n -> n\n    | _ -> 0\nlet g y =\n    match y with\n    | n when isEven n -> n\n    | _ -> 1"

    Assert.Equal(1, List.length suggestions)

[<Fact>]
let ``an overloaded method guard annotates the extracted input`` () =
    // from Fuuga: Path.IsPathRooted takes string OR ReadOnlySpan<char>; the
    // extracted pattern's `input` has no inference context, so the resolved
    // parameter type is spelled out
    match
        findIn
            "module Test\nopen System.IO\nlet f (p: string) =\n    match p with\n    | q when Path.IsPathRooted q -> q\n    | q -> q"
    with
    | [ s ] ->
        Assert.Contains("(input: string)", s.InsertText)

        let patched = applyEdit "module Test\nopen System.IO\nlet f (p: string) =\n    match p with\n    | q when Path.IsPathRooted q -> q\n    | q -> q" s.ClauseRange s.ClauseText
        let patched = applyEdit patched s.InsertRange s.InsertText
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one annotated suggestion, got %A" other
