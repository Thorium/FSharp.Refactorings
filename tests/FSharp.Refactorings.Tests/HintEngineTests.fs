module FSharp.Refactorings.Tests.HintEngineTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

let private findWith (extraRules: string list) (source: string) =
    let tree, sourceText = parse source
    HintEngine.find extraRules tree sourceText

let private findIn (source: string) = findWith [] source

let private assertSingleSuggestion (source: string) (expectedReplacement: string) =
    match findIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(parsesCleanly patched, sprintf "Patched source does not parse:\n%s" patched)
    | other -> failwithf "Expected exactly one suggestion, got %d: %A" (List.length other) other

let private assertNoSuggestion (source: string) = Assert.Empty(findIn source)

[<Fact>]
let ``negated equality flips the operator`` () =
    assertSingleSuggestion "module Test\nlet f a b = not (a = b)" "a <> b"

[<Fact>]
let ``negated less-than flips to greater-or-equal`` () =
    assertSingleSuggestion "module Test\nlet f (a: int) b = not (a < b)" "a >= b"

[<Fact>]
let ``metavariables bind complex expressions with parens as needed`` () =
    assertSingleSuggestion "module Test\nlet f (g: int -> int) x b = not (g x = b)" "(g x) <> b"

[<Fact>]
let ``bool comparison with true is dropped`` () =
    assertSingleSuggestion "module Test\nlet f (x: bool) = x = true" "x"

[<Fact>]
let ``bool comparison with false negates`` () =
    assertSingleSuggestion "module Test\nlet f (x: bool) = false = x" "not x"

[<Fact>]
let ``null comparison becomes isNull`` () =
    assertSingleSuggestion "module Test\nlet f (s: string) = s = null" "isNull s"

[<Fact>]
let ``null inequality becomes not isNull`` () =
    assertSingleSuggestion "module Test\nlet f (s: string) = s <> null" "not (isNull s)"

[<Fact>]
let ``map-map fusion composes the mappers`` () =
    assertSingleSuggestion "module Test\nlet f g h xs = List.map g (List.map h xs)" "List.map (h >> g) xs"

[<Fact>]
let ``concat of map becomes collect`` () =
    assertSingleSuggestion
        "module Test\nlet f (g: int -> int list) xs = List.concat (List.map g xs)"
        "List.collect g xs"

[<Fact>]
let ``isEmpty of filter becomes not exists`` () =
    assertSingleSuggestion
        "module Test\nlet f (p: int -> bool) xs = Seq.isEmpty (Seq.filter p xs)"
        "not (Seq.exists p xs)"

[<Fact>]
let ``not isEmpty of filter becomes exists`` () =
    assertSingleSuggestion
        "module Test\nlet f (p: int -> bool) xs = not (List.isEmpty (List.filter p xs))"
        "List.exists p xs"

[<Fact>]
let ``fold plus zero becomes sum`` () =
    assertSingleSuggestion "module Test\nlet f (xs: int list) = List.fold (+) 0 xs" "List.sum xs"

[<Fact>]
let ``sum of map becomes sumBy`` () =
    assertSingleSuggestion "module Test\nlet f (g: int -> int) xs = List.sum (List.map g xs)" "List.sumBy g xs"

[<Fact>]
let ``map id disappears`` () =
    assertSingleSuggestion "module Test\nlet f (xs: int list) = List.map id xs" "xs"

[<Fact>]
let ``head of sort becomes min`` () =
    assertSingleSuggestion "module Test\nlet f (xs: int list) = List.head (List.sort xs)" "List.min xs"

[<Fact>]
let ``compare equals zero becomes equality`` () =
    assertSingleSuggestion "module Test\nlet f (a: int) b = compare a b = 0" "a = b"

[<Fact>]
let ``double rev disappears`` () =
    assertSingleSuggestion "module Test\nlet f (xs: int list) = List.rev (List.rev xs)" "xs"

[<Fact>]
let ``id composition simplifies`` () =
    assertSingleSuggestion "module Test\nlet f (g: int -> int) = id >> g" "g"

[<Fact>]
let ``repeated metavariable must bind identical text`` () =
    // rev(rev) with different arguments must not match the double-rev rule
    assertNoSuggestion "module Test\nlet f (xs: int list) ys = List.rev (List.append (List.rev ys) xs)"

[<Fact>]
let ``replacement in operand position is parenthesized`` () =
    let src = "module Test\nlet f (b: bool) (n: int) = string (not (not b))"
    // inner not(not b) matches; parent is a Paren so no extra parens needed
    match findIn src with
    | [ s ] -> Assert.Equal("b", s.ReplacementText)
    | other -> failwithf "Expected one operand-position suggestion, got %A" other

[<Fact>]
let ``extra rules from configuration are applied`` () =
    let suggestions =
        findWith
            [ "Option.isSome x |> not ===> Option.isNone x" ]
            "module Test\nlet f (x: int option) = Option.isSome x |> not"

    match suggestions with
    | [ s ] -> Assert.Equal("Option.isNone x", s.ReplacementText)
    | other -> failwithf "Expected one extra-rule suggestion, got %A" other

[<Fact>]
let ``invalid extra rules are skipped silently`` () =
    Assert.Empty(findWith [ "not valid ==> nope"; "also (((" ] "module Test\nlet f (x: int) = x")

[<Fact>]
let ``rule dropping an effectful binding does not fire`` () =
    // `true && g ()` -> `g ()` is fine (kept); but a rule that DROPS a
    // non-atom must not fire: craft one via extra rules
    Assert.Empty(findWith [ "ignore x ===> ()" ] "module Test\nlet f (g: unit -> int) = ignore (g ())")

[<Fact>]
let ``rule dropping a pure atom fires`` () =
    let suggestions =
        findWith [ "ignore x ===> ()" ] "module Test\nlet f (n: int) = ignore n"

    match suggestions with
    | [ s ] -> Assert.Equal("()", s.ReplacementText)
    | other -> failwithf "Expected one pure-atom suggestion, got %A" other

[<Fact>]
let ``multi-line expressions are not matched`` () =
    assertNoSuggestion "module Test\nlet f a b =\n    not (\n        a = b\n    )"

[<Fact>]
let ``named arguments are never rewritten`` () =
    // found by running the engine on our own code: `Foo(Flag = true)` parses
    // as an equality expression but is a named argument
    assertNoSuggestion "module Test\nlet f () = System.Text.Json.JsonDocumentOptions(AllowTrailingCommas = true)"

[<Fact>]
let ``named argument in a multi-argument call is never rewritten`` () =
    assertNoSuggestion
        "module Test\nlet f () = System.Text.Json.JsonDocumentOptions(CommentHandling = System.Text.Json.JsonCommentHandling.Skip, AllowTrailingCommas = true)"

// ---- harder cases ----

[<Fact>]
let ``record field assignment is not a comparison`` () =
    // `{ r with Flag = true }` must not become `{ r with Flag }`
    assertNoSuggestion "module Test\ntype R = { Flag: bool; N: int }\nlet f (r: R) = { r with Flag = true }"

[<Fact>]
let ``quoted code is never rewritten`` () =
    // rewriting inside <@ ... @> changes the reified AST
    assertNoSuggestion "module Test\nlet q (x: bool) = <@ x = true @>"

[<Fact>]
let ``metavariables inside array literals substitute correctly`` () =
    // regression: Sequential chains inside [| ... |] were not traversed
    assertSingleSuggestion
        "module Test\nlet f (p: int[]) (q: int[]) (r: int[]) = Array.append p (Array.append q r)"
        "Array.concat [| p; q; r |]"

[<Fact>]
let ``match nested inside surrounding calls still rewrites precisely`` () =
    // fusion target sits inside a larger expression with intermediate steps
    assertSingleSuggestion
        "module Test\nlet f (g: int -> int) xs = Set.ofList (List.map string (List.map g xs))"
        "List.map (g >> string) xs"

[<Fact>]
let ``pipelined form of an application rule is normalized and matched`` () =
    // `lhs |> rhs` unifies with application-shaped rules as `rhs lhs`
    assertSingleSuggestion
        "module Test\nlet f (g: int -> int) xs = xs |> List.map g |> List.map string"
        "List.map (g >> string) xs"

[<Fact>]
let ``pipe normalization also simplifies inner pipeline stages`` () =
    // the inner `xs |> List.map id` matches `List.map id x ===> x`
    assertSingleSuggestion "module Test\nlet f (xs: string list) = xs |> List.map id |> List.length" "xs"

[<Fact>]
let ``De Morgan combines negated conjuncts`` () =
    assertSingleSuggestion "module Test\nlet f (a: bool) b = not a && not b" "not (a || b)"

[<Fact>]
let ``De Morgan combines negated disjuncts`` () =
    assertSingleSuggestion "module Test\nlet f (a: bool) b = not a || not b" "not (a && b)"
