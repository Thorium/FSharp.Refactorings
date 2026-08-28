module FSharp.Refactor.Tests.CharOverloadTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private charOverloadsIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    CharOverload.find tree sourceText checkResults

let private assertCharFix (source: string) (expectedReplacement: string) =
    match charOverloadsIn source with
    | [ s ] ->
        match s.ReplacementText with
        | Some replacement ->
            Assert.Equal(expectedReplacement, replacement)
            let patched = applyEdit source s.Range replacement
            Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
        | None -> failwith "Expected a fix, got an advisory"
    | other -> failwithf "Expected exactly one char-overload suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``Contains with a single-char string gets the char fix`` () =
    assertCharFix "let f (s: string) = s.Contains \"x\"" "'x'"

[<Fact>]
let ``StringBuilder Append gets the char fix`` () =
    assertCharFix "let f (sb: System.Text.StringBuilder) = sb.Append(\"x\")" "'x'"

[<Fact>]
let ``ordinal StartsWith collapses to the char overload`` () =
    assertCharFix "let f (s: string) = s.StartsWith(\"x\", System.StringComparison.Ordinal)" "('x')"

[<Fact>]
let ``quote character is escaped in the char literal`` () =
    assertCharFix "let f (s: string) = s.Contains \"'\"" "'\\''"

[<Fact>]
let ``bare EndsWith stays advisory because of culture semantics`` () =
    match charOverloadsIn "let f (s: string) = s.EndsWith \"x\"" with
    | [ s ] ->
        Assert.Equal(None, s.ReplacementText)
        Assert.Equal("EndsWith", s.MethodName)
    | other -> failwithf "Expected exactly one advisory, got %A" other

[<Fact>]
let ``multi-character strings are left alone`` () =
    Assert.Empty(charOverloadsIn "let f (s: string) = s.Contains \"xy\"")

[<Fact>]
let ``list Contains is not the string method`` () =
    Assert.Empty(charOverloadsIn "let f (xs: System.Collections.Generic.List<string>) = xs.Contains \"x\"")

[<Fact>]
let ``a verbatim single-char string is the char overload too`` () =
    // @"\" is THE spelling of a backslash in path code — the FR0015 lesson.
    // Contains, because StartsWith(string) is culture-sensitive and only
    // ever gets the advisory tier
    assertCharFix "let f (s: string) = s.Contains @\"\\\"" "'\\\\'"
