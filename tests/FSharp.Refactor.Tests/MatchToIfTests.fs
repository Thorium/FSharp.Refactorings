module FSharp.Refactor.Tests.MatchToIfTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private findIn (source: string) =
    let tree, sourceText = parse source
    MatchToIf.find tree sourceText

/// Run the refactoring on a source expected to yield exactly one suggestion,
/// verify the replacement text, and verify the patched file still parses.
let private assertSingleSuggestion (source: string) (expectedReplacement: string) =
    match findIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(parsesCleanly patched, sprintf "Patched source does not parse:\n%s" patched)
    | other -> failwithf "Expected exactly one suggestion, got %d: %A" (List.length other) other

let private assertNoSuggestion (source: string) = Assert.Empty(findIn source)

[<Fact>]
let ``single-line true-false match`` () =
    assertSingleSuggestion "module Test\nlet f x = match x with | true -> 1 | false -> 2" "if x then 1 else 2"

[<Fact>]
let ``multi-line true-false match`` () =
    assertSingleSuggestion
        "module Test\nlet f x =\n    match x with\n    | true -> 1\n    | false -> 2"
        "if x then 1 else 2"

[<Fact>]
let ``false clause first swaps the branches`` () =
    assertSingleSuggestion "module Test\nlet f x = match x with | false -> 0 | true -> 1" "if x then 1 else 0"

[<Fact>]
let ``true and wildcard`` () =
    assertSingleSuggestion "module Test\nlet f x = match x with | true -> 1 | _ -> 2" "if x then 1 else 2"

[<Fact>]
let ``false and wildcard swaps the branches`` () =
    assertSingleSuggestion "module Test\nlet f x = match x with | false -> 0 | _ -> 1" "if x then 1 else 0"

[<Fact>]
let ``complex scrutinee expression is kept verbatim`` () =
    assertSingleSuggestion
        "module Test\nlet f a b = match a + 1 = b with | true -> \"eq\" | false -> \"ne\""
        "if a + 1 = b then \"eq\" else \"ne\""

[<Fact>]
let ``match nested in larger expression`` () =
    assertSingleSuggestion "module Test\nlet f x = 1 + (match x with | true -> 1 | false -> 2)" "if x then 1 else 2"

[<Fact>]
let ``when guard is not rewritten`` () =
    assertNoSuggestion "module Test\nlet f x y = match x with | true when y > 0 -> 1 | _ -> 2"

[<Fact>]
let ``non-boolean patterns are not rewritten`` () =
    assertNoSuggestion "module Test\nlet f x = match x with | 1 -> 1 | _ -> 2"

[<Fact>]
let ``three clauses are not rewritten`` () =
    assertNoSuggestion "module Test\nlet f x = match x with | true -> 1 | false -> 2 | _ -> 3"

[<Fact>]
let ``named pattern instead of wildcard is not rewritten`` () =
    // `other` binds a value; rewriting would lose the binding
    assertNoSuggestion "module Test\nlet f x = match x with | true -> 1 | other -> 2"

[<Fact>]
let ``multi-line branch body is not rewritten`` () =
    assertNoSuggestion
        "module Test\nlet f x =\n    match x with\n    | true ->\n        let y = 1\n        y + 1\n    | false -> 2"

[<Fact>]
let ``parenthesized lambda application in branch is rewritten`` () =
    // the lambda is parenthesized, so inlining it is safe
    assertSingleSuggestion
        "module Test\nlet f x = match x with | true -> (fun a -> a) 1 | false -> id 2"
        "if x then (fun a -> a) 1 else id 2"

[<Fact>]
let ``pipe-left lambda branch body is not rewritten`` () =
    assertNoSuggestion "module Test\nlet f x g =\n    match x with\n    | true -> g <| fun a -> a\n    | false -> g id"

[<Fact>]
let ``nested if branch body is not rewritten`` () =
    assertNoSuggestion "module Test\nlet f x y =\n    match x with\n    | true -> if y then 1 else 2\n    | false -> 3"

[<Fact>]
let ``parenthesized nested if branch body is rewritten`` () =
    assertSingleSuggestion
        "module Test\nlet f x y = match x with | true -> (if y then 1 else 2) | false -> 3"
        "if x then (if y then 1 else 2) else 3"

[<Fact>]
let ``let binding in branch is not rewritten`` () =
    assertNoSuggestion "module Test\nlet f x =\n    match x with\n    | true -> let y = 1 in y\n    | false -> 2"

[<Fact>]
let ``two independent matches produce two suggestions`` () =
    let source =
        "module Test\nlet f x = match x with | true -> 1 | false -> 2\nlet g y = match y with | true -> 3 | false -> 4"

    let suggestions = findIn source
    Assert.Equal(2, List.length suggestions)

[<Fact>]
let ``a match spanning conditional compilation stays`` () =
    // corpus regression: the tree only sees the active #if branch; the fix
    // would splice out the directives and break the inactive branch
    assertNoSuggestion
        "module Test\nlet f (p: string) =\n#if SOMEDEFINE\n    match p.Length > 0 with\n#else\n    match p.Length > 1 with\n#endif\n    | true -> p\n    | false -> \"\""
