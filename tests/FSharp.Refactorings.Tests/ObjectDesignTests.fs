module FSharp.Refactorings.Tests.ObjectDesignTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

// ---- FR0031 StringConcat ----

let private concatIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    StringConcat.find tree sourceText checkResults

let private assertConcat (source: string) (expectedReplacement: string) =
    match concatIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one concat suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``literal and identifier chain becomes interpolation`` () =
    assertConcat "let f (name: string) = \"Hello \" + name + \"!\"" "$\"Hello {name}!\""

[<Fact>]
let ``dotted string property is a hole`` () =
    assertConcat
        "type P = { Label: string }\nlet f (prefix: string) (p: P) = prefix + \": \" + p.Label"
        "$\"{prefix}: {p.Label}\""

[<Fact>]
let ``braces or percent in a literal leave the chain alone`` () =
    // $"{{100%%}} {name}" reads worse than the concatenation it replaces
    Assert.Empty(concatIn "let f (name: string) = \"{100%} \" + name")

[<Fact>]
let ``non-string operand chain is left alone`` () =
    Assert.Empty(concatIn "let f (n: int) = n + 1")

[<Fact>]
let ``method-call operand is left alone`` () =
    Assert.Empty(concatIn "let f (name: string) = \"Hello \" + name.Trim()")

[<Fact>]
let ``all-literal chain is left alone`` () =
    Assert.Empty(concatIn "let f () = \"a\" + \"b\"")

[<Fact>]
let ``literal-free chain is left alone`` () =
    Assert.Empty(concatIn "let f (a: string) (b: string) = a + b")

// ---- FR0032 / FR0033 ObjectDesign ----

let private designIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    ObjectDesign.find tree sourceText checkResults

[<Fact>]
let ``new-constructed disposable field without IDisposable is noted`` () =
    let disposables, _ =
        designIn "type Holder() =\n    let stream = new System.IO.MemoryStream()\n    member _.Size = stream.Length"

    match disposables with
    | [ s ] ->
        Assert.Equal("Holder", s.TypeName)
        Assert.Equal("stream", s.FieldName)
    | other -> failwithf "Expected exactly one disposable-field note, got %A" other

[<Fact>]
let ``implementing IDisposable silences the field note`` () =
    let disposables, _ =
        designIn
            "type Holder() =\n    let stream = new System.IO.MemoryStream()\n    member _.Size = stream.Length\n\n    interface System.IDisposable with\n        member _.Dispose() = stream.Dispose()"

    Assert.Empty disposables

[<Fact>]
let ``injected disposable is not owned`` () =
    let disposables, _ =
        designIn "type Holder(stream: System.IO.MemoryStream) =\n    let s = stream\n    member _.Size = s.Length"

    Assert.Empty disposables

[<Fact>]
let ``member without instance state can be static`` () =
    let _, statics =
        designIn
            "type Calc(seed: int) =\n    let offset = seed * 2\n    member _.Twice(x: int) = x * 2\n    member _.WithOffset(x: int) = x + offset"

    match statics with
    | [ s ] -> Assert.Equal("Twice", s.MemberName)
    | other -> failwithf "Expected exactly one static-member note, got %A" other

[<Fact>]
let ``member using the self identifier stays instance`` () =
    let _, statics =
        designIn "type Calc() =\n    member this.Twice(x: int) = this.Base + x\n    member _.Base = 2"

    Assert.Empty statics

[<Fact>]
let ``member using a constructor parameter stays instance`` () =
    let _, statics =
        designIn "type Calc(seed: int) =\n    member _.Offset(x: int) = x + seed"

    Assert.Empty statics

[<Fact>]
let ``override members are never suggested static`` () =
    let _, statics = designIn "type Desc() =\n    override _.ToString() = \"desc\""

    Assert.Empty statics

[<Fact>]
let ``member parameter shadowing a field still counts as static`` () =
    let _, statics =
        designIn
            "type Calc() =\n    let offset = 2\n    member _.Apply(offset: int) = offset + 1\n    member _.Off = offset"

    match statics with
    | [ s ] -> Assert.Equal("Apply", s.MemberName)
    | other -> failwithf "Expected exactly one shadowed-param note, got %A" other
