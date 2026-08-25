module FSharp.Refactorings.Tests.LoopPerfTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

// ---- FR0035 / FR0037 LoopPerf ----

let private loopPerfIn (source: string) =
    let tree, sourceText = parse source
    LoopPerf.find tree sourceText

[<Fact>]
let ``contains inside a for loop is noted`` () =
    let contains, _ =
        loopPerfIn
            "module Test\nlet f (xs: int list) (ys: int list) =\n    for x in xs do\n        if List.contains x ys then printfn \"%d\" x"

    match contains with
    | [ s ] ->
        Assert.Equal("ys", s.CollectionName)
        Assert.Equal("List", s.ModuleName)
    | other -> failwithf "Expected exactly one contains note, got %A" other

[<Fact>]
let ``piped contains inside a filter callback is noted`` () =
    let contains, _ =
        loopPerfIn
            "module Test\nlet f (xs: int list) (ys: int list) = xs |> List.filter (fun x -> ys |> List.contains x)"

    match contains with
    | [ s ] -> Assert.Equal("ys", s.CollectionName)
    | other -> failwithf "Expected exactly one callback contains note, got %A" other

[<Fact>]
let ``contains outside any loop is fine`` () =
    let contains, _ =
        loopPerfIn "module Test\nlet f (x: int) (ys: int list) = List.contains x ys"

    Assert.Empty contains

[<Fact>]
let ``probing the loop variable itself is fine`` () =
    // scanning each inner collection once is not a repeated probe
    let contains, _ =
        loopPerfIn
            "module Test\nlet f (xss: int list list) =\n    for xs in xss do\n        if List.contains 1 xs then printfn \"hit\""

    Assert.Empty contains

[<Fact>]
let ``ConcurrentDictionary built in a loop is noted`` () =
    let _, constructions =
        loopPerfIn
            "module Test\nopen System.Collections.Concurrent\nlet f (xs: int list) =\n    for x in xs do\n        let d = ConcurrentDictionary<int, int>()\n        d.TryAdd(x, x) |> ignore"

    match constructions with
    | [ s ] -> Assert.Equal("ConcurrentDictionary", s.TypeName)
    | other -> failwithf "Expected exactly one construction note, got %A" other

[<Fact>]
let ``JsonSerializerOptions built in a loop is noted`` () =
    let _, constructions =
        loopPerfIn
            "module Test\nopen System.Text.Json\nlet f (xs: string list) =\n    for x in xs do\n        let opts = JsonSerializerOptions()\n        ignore (JsonSerializer.Deserialize<int>(x, opts))"

    match constructions with
    | [ s ] -> Assert.Equal("JsonSerializerOptions", s.TypeName)
    | other -> failwithf "Expected exactly one options-construction note, got %A" other

[<Fact>]
let ``SearchValues Create in a loop is noted`` () =
    let _, constructions =
        loopPerfIn
            "module Test\nopen System.Buffers\nlet f (xs: string list) =\n    for x in xs do\n        let sv = SearchValues.Create \"aeiou\"\n        ignore (x.AsSpan().IndexOfAny sv)"

    match constructions with
    | [ s ] -> Assert.Equal("SearchValues", s.TypeName)
    | other -> failwithf "Expected exactly one SearchValues note, got %A" other

[<Fact>]
let ``ConcurrentDictionary outside a loop is fine`` () =
    let _, constructions =
        loopPerfIn
            "module Test\nopen System.Collections.Concurrent\nlet d = ConcurrentDictionary<int, int>()\nlet f (x: int) = d.TryAdd(x, x) |> ignore"

    Assert.Empty constructions

// ---- FR0036 TypeChecks ----

let private typeChecksIn (source: string) =
    let tree, sourceText = parse source
    TypeChecks.find tree sourceText

[<Fact>]
let ``type name string comparison is noted`` () =
    let suggestions =
        typeChecksIn "module Test\nlet f (x: obj) = x.GetType().Name = \"Customer\""

    match suggestions with
    | [ s ] -> Assert.Equal(TypeChecks.TypeCheckKind.NameComparison "Name", s.Kind)
    | other -> failwithf "Expected exactly one name-comparison note, got %A" other

[<Fact>]
let ``full name comparison is noted either way round`` () =
    let suggestions =
        typeChecksIn "module Test\nlet f (x: obj) = \"N.Customer\" = x.GetType().FullName"

    match suggestions with
    | [ s ] -> Assert.Equal(TypeChecks.TypeCheckKind.NameComparison "FullName", s.Kind)
    | other -> failwithf "Expected exactly one full-name note, got %A" other

[<Fact>]
let ``GetType equality with typeof is noted`` () =
    let suggestions =
        typeChecksIn "module Test\nlet f (x: obj) = x.GetType() = typeof<string>"

    match suggestions with
    | [ s ] -> Assert.Equal(TypeChecks.TypeCheckKind.TypeofEquality("x", "string"), s.Kind)
    | other -> failwithf "Expected exactly one typeof-equality note, got %A" other

[<Fact>]
let ``comparing two GetType calls is fine`` () =
    Assert.Empty(typeChecksIn "module Test\nlet f (x: obj) (y: obj) = x.GetType() = y.GetType()")

[<Fact>]
let ``typeof against typeof is fine`` () =
    Assert.Empty(typeChecksIn "module Test\nlet f () = typeof<string> = typeof<obj>")
