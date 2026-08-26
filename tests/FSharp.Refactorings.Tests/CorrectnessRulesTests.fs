module FSharp.Refactorings.Tests.CorrectnessRulesTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

// ---- FR0044 Reraise ----

let private reraiseIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    Reraise.find tree sourceText checkResults

let private assertReraise (source: string) =
    match reraiseIn source with
    | [ s ] ->
        let patched = applyEdit source s.Range "reraise ()"
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one reraise suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``raise of the caught exception becomes reraise`` () =
    assertReraise
        "let f (act: unit -> int) =\n    try act ()\n    with ex ->\n        printfn \"%s\" ex.Message\n        raise ex"

[<Fact>]
let ``typed catch with as-binding is also covered`` () =
    assertReraise
        "let f (act: unit -> int) =\n    try act ()\n    with :? System.IO.IOException as ex ->\n        printfn \"%s\" ex.Message\n        raise ex"

[<Fact>]
let ``raising a different exception is intentional`` () =
    Assert.Empty(
        reraiseIn
            "let f (act: unit -> int) =\n    try act ()\n    with ex ->\n        raise (System.InvalidOperationException(\"wrap\", ex))"
    )

[<Fact>]
let ``raise inside a lambda cannot become reraise`` () =
    // reraise () does not compile inside a closure
    Assert.Empty(
        reraiseIn
            "let f (act: unit -> int) (defer: (unit -> int) -> int) =\n    try act ()\n    with ex -> defer (fun () -> raise ex)"
    )

[<Fact>]
let ``raise inside a nested handler refers to the inner exception`` () =
    Assert.Empty(
        reraiseIn
            "let f (act: unit -> int) (cleanup: unit -> int) =\n    try act ()\n    with ex ->\n        try cleanup ()\n        with _ -> raise ex"
    )

// ---- FR0045 NaNComparison ----

let private nanIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    NaNComparison.find tree sourceText checkResults

let private assertNaN (source: string) (expectedReplacement: string) =
    match nanIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one NaN suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``equality with nan becomes IsNaN`` () =
    assertNaN "let f (x: float) = x = nan" "System.Double.IsNaN x"

[<Fact>]
let ``inequality with Double NaN becomes negated IsNaN`` () =
    assertNaN "let f (x: float) = x <> System.Double.NaN" "not (System.Double.IsNaN x)"

[<Fact>]
let ``reversed operand order is covered`` () =
    assertNaN "let f (x: float) = nan = x" "System.Double.IsNaN x"

[<Fact>]
let ``single NaN uses Single IsNaN`` () =
    assertNaN "let f (x: float32) = x = System.Single.NaN" "System.Single.IsNaN x"

[<Fact>]
let ``ordinary float equality is left alone`` () =
    Assert.Empty(nanIn "let f (x: float) (y: float) = x = y")

// ---- FR0046 WeakLock ----

let private locksIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    WeakLock.find tree sourceText checkResults

[<Fact>]
let ``locking on a string literal is noted`` () =
    match locksIn "let f () = lock \"cache\" (fun () -> 1)" with
    | [ s ] -> Assert.Equal(WeakLock.WeakKind.StringValue, s.Kind)
    | other -> failwithf "Expected exactly one weak-lock note, got %A" other

[<Fact>]
let ``locking on typeof is noted`` () =
    match locksIn "let f () = lock typeof<string> (fun () -> 1)" with
    | [ s ] -> Assert.Equal(WeakLock.WeakKind.TypeObject, s.Kind)
    | other -> failwithf "Expected exactly one typeof-lock note, got %A" other

[<Fact>]
let ``locking on a dedicated object is fine`` () =
    Assert.Empty(locksIn "let lockObj = obj ()\nlet f () = lock lockObj (fun () -> 1)")

// ---- FR0047 UndisposedField ----

let private designIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    ObjectDesign.find tree sourceText checkResults

[<Fact>]
let ``disposable field missing from Dispose is noted`` () =
    let _, _, undisposed =
        designIn
            "type Holder() =\n    let stream = new System.IO.MemoryStream()\n    let backup = new System.IO.MemoryStream()\n    member _.Size = stream.Length + backup.Length\n\n    interface System.IDisposable with\n        member _.Dispose() = stream.Dispose()"

    match undisposed with
    | [ s ] -> Assert.Equal("backup", s.FieldName)
    | other -> failwithf "Expected exactly one undisposed-field note, got %A" other

[<Fact>]
let ``fields the Dispose touches are fine`` () =
    let _, _, undisposed =
        designIn
            "type Holder() =\n    let stream = new System.IO.MemoryStream()\n    member _.Size = stream.Length\n\n    interface System.IDisposable with\n        member _.Dispose() = stream.Dispose()"

    Assert.Empty undisposed

// ---- FR0048 FormatArgs ----

let private formatsIn (source: string) =
    let tree, sourceText = parse source
    FormatArgs.find tree sourceText

[<Fact>]
let ``missing format argument is noted`` () =
    match formatsIn "module Test\nlet f (x: int) = System.String.Format(\"{0} of {1}\", x)" with
    | [ s ] ->
        Assert.Equal(1, s.MissingIndex)
        Assert.Equal(1, s.ArgCount)
    | other -> failwithf "Expected exactly one format note, got %A" other

[<Fact>]
let ``matching arguments are fine`` () =
    Assert.Empty(formatsIn "module Test\nlet f (x: int) (y: int) = System.String.Format(\"{0} of {1}\", x, y)")

[<Fact>]
let ``escaped braces are not placeholders`` () =
    Assert.Empty(formatsIn "module Test\nlet f (x: int) = System.String.Format(\"{{0}} literal {0}\", x)")
