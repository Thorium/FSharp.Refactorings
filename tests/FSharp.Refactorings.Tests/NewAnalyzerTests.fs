module FSharp.Refactorings.Tests.NewAnalyzerTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

// ---- FR0015 RegexUsage ----

let private regexIn (source: string) =
    let tree, sourceText = parse source
    RegexUsage.find tree sourceText

let private assertRegexFix (source: string) (expectedReplacement: string) =
    match regexIn source with
    | [ s ] ->
        Assert.Equal(RegexUsage.RegexSuggestionKind.StringOperation, s.Kind)

        match s.Edits with
        | [ (range, _, replacement) ] ->
            Assert.Equal(expectedReplacement, replacement)
            let patched = applyEdit source range replacement
            Assert.True(parsesCleanly patched, sprintf "Patched source does not parse:\n%s" patched)
        | other -> failwithf "Expected exactly one edit, got %A" other
    | other -> failwithf "Expected exactly one regex suggestion, got %d: %A" (List.length other) other

/// Apply a hoist suggestion's edits bottom-up and verify the patched text.
let private assertRegexHoist (source: string) (expectedPatched: string) =
    match regexIn source with
    | [ s ] ->
        Assert.Equal(RegexUsage.RegexSuggestionKind.HoistFromLoop, s.Kind)

        let patched =
            s.Edits
            |> List.sortByDescending (fun (r, _, _) -> r.StartLine, r.StartColumn)
            |> List.fold (fun acc (r, _, t) -> applyEdit acc r t) source

        Assert.Equal(expectedPatched, patched)
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one hoist suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``anchored-start literal becomes StartsWith`` () =
    assertRegexFix
        "module Test\nopen System.Text.RegularExpressions\nlet f (s: string) = Regex.IsMatch(s, \"^abc\")"
        "s.StartsWith \"abc\""

[<Fact>]
let ``anchored-end literal becomes EndsWith`` () =
    assertRegexFix
        "module Test\nopen System.Text.RegularExpressions\nlet f (s: string) = Regex.IsMatch(s, \"abc$\")"
        "s.EndsWith \"abc\""

[<Fact>]
let ``unanchored literal becomes Contains`` () =
    assertRegexFix
        "module Test\nopen System.Text.RegularExpressions\nlet f (s: string) = Regex.IsMatch(s, \"abc\")"
        "s.Contains \"abc\""

[<Fact>]
let ``pattern with metacharacters is left alone`` () =
    Assert.Empty(
        regexIn "module Test\nopen System.Text.RegularExpressions\nlet f (s: string) = Regex.IsMatch(s, \"a.c\")"
    )

[<Fact>]
let ``escaped dollar is not an anchor`` () =
    Assert.Empty(
        regexIn "module Test\nopen System.Text.RegularExpressions\nlet f (s: string) = Regex.IsMatch(s, \"abc\\\\$\")"
    )

[<Fact>]
let ``fully anchored pattern is left alone`` () =
    Assert.Empty(
        regexIn "module Test\nopen System.Text.RegularExpressions\nlet f (s: string) = Regex.IsMatch(s, \"^abc$\")"
    )

[<Fact>]
let ``regex call in a loop is hoisted above the declaration`` () =
    assertRegexHoist
        "module Test\nopen System.Text.RegularExpressions\nlet f (xs: string list) =\n    for s in xs do\n        if Regex.IsMatch(s, \"a.c\") then printfn \"%s\" s"
        "module Test\nopen System.Text.RegularExpressions\nlet private acRegex = Regex \"a.c\"\nlet f (xs: string list) =\n    for s in xs do\n        if acRegex.IsMatch s then printfn \"%s\" s"

[<Fact>]
let ``hoist without the open stays advice-only`` () =
    let source =
        "module Test\nlet f (xs: string list) =\n    for s in xs do\n        if System.Text.RegularExpressions.Regex.IsMatch(s, \"a.c\") then printfn \"%s\" s"

    match regexIn source with
    | [ s ] ->
        Assert.Equal(RegexUsage.RegexSuggestionKind.HoistFromLoop, s.Kind)
        Assert.Empty s.Edits
    | other -> failwithf "Expected exactly one advice-only hoist, got %A" other

[<Fact>]
let ``regex Replace in a loop is hoisted with both remaining arguments`` () =
    assertRegexHoist
        "module Test\nopen System.Text.RegularExpressions\nlet f (xs: string list) =\n    for s in xs do\n        printfn \"%s\" (Regex.Replace(s, \"a.c\", \"-\"))"
        "module Test\nopen System.Text.RegularExpressions\nlet private acRegex = Regex \"a.c\"\nlet f (xs: string list) =\n    for s in xs do\n        printfn \"%s\" (acRegex.Replace(s, \"-\"))"

[<Fact>]
let ``literal match in a loop reports only the string operation`` () =
    let source =
        "module Test\nopen System.Text.RegularExpressions\nlet f (xs: string list) =\n    for s in xs do\n        if Regex.IsMatch(s, \"abc\") then printfn \"%s\" s"

    match regexIn source with
    | [ s ] -> Assert.Equal(RegexUsage.RegexSuggestionKind.StringOperation, s.Kind)
    | other -> failwithf "Expected exactly one string-op suggestion, got %A" other

[<Fact>]
let ``instance regex call outside a loop is not flagged`` () =
    Assert.Empty(
        regexIn
            "module Test\nopen System.Text.RegularExpressions\nlet r = Regex \"a.c\"\nlet f (s: string) = r.IsMatch s"
    )

// ---- FR0016 StructDu ----

let private structDuIn (source: string) =
    let tree, sourceText = parse source
    StructDu.find tree sourceText

let private assertStructDu (source: string) (expectedPatched: string) =
    match structDuIn source with
    | [ s ] ->
        let patched = applyEdit source s.InsertRange s.InsertText
        Assert.Equal(expectedPatched, patched)
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one struct-DU suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``small named-field union gains the attribute`` () =
    assertStructDu
        "module Test\ntype Shape =\n    | Circle of radius: float\n    | Square of side: float"
        "module Test\n[<Struct>]\ntype Shape =\n    | Circle of radius: float\n    | Square of side: float"

[<Fact>]
let ``single fielded case may be unnamed`` () =
    assertStructDu
        "module Test\ntype Id =\n    | Id of int\n    | Missing"
        "module Test\n[<Struct>]\ntype Id =\n    | Id of int\n    | Missing"

[<Fact>]
let ``string fields are not small value types`` () =
    Assert.Empty(structDuIn "module Test\ntype T =\n    | A of string\n    | B of int")

[<Fact>]
let ``recursive union is excluded by the whitelist`` () =
    Assert.Empty(structDuIn "module Test\ntype Tree =\n    | Leaf of int\n    | Node of Tree")

[<Fact>]
let ``two cases with unnamed fields are excluded`` () =
    // compiled ItemN names would collide in a struct union
    Assert.Empty(structDuIn "module Test\ntype T =\n    | A of int\n    | B of float")

[<Fact>]
let ``existing attributes are left alone`` () =
    Assert.Empty(structDuIn "module Test\n[<Struct>]\ntype T =\n    | A of a: int\n    | B of b: float")

[<Fact>]
let ``all-nullary union is not suggested`` () =
    Assert.Empty(structDuIn "module Test\ntype T =\n    | A\n    | B")

// ---- FR0017 AsyncIgnore ----

let private discardedAsyncIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    AsyncIgnore.find tree sourceText checkResults

[<Fact>]
let ``piped async into ignore is flagged`` () =
    let suggestions = discardedAsyncIn "let f (comp: Async<int>) = comp |> ignore"

    match suggestions with
    | [ s ] -> Assert.Equal("comp", s.Name)
    | other -> failwithf "Expected exactly one async-ignore suggestion, got %A" other

[<Fact>]
let ``direct ignore application is flagged`` () =
    let suggestions = discardedAsyncIn "let f (comp: Async<int>) = ignore comp"

    match suggestions with
    | [ s ] -> Assert.Equal("comp", s.Name)
    | other -> failwithf "Expected exactly one direct-ignore suggestion, got %A" other

[<Fact>]
let ``ignoring a non-async value is fine`` () =
    Assert.Empty(discardedAsyncIn "let f (n: int) = n |> ignore")

[<Fact>]
let ``Async.Ignore usage is not flagged`` () =
    Assert.Empty(discardedAsyncIn "let f (comp: Async<int>) = async { do! comp |> Async.Ignore }")
