module FSharp.Refactorings.Tests.QueryInLoopTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

let private queriesIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    QueryInLoop.find tree sourceText checkResults

[<Fact>]
let ``queryable iterated inside a loop is noted`` () =
    let suggestions =
        queriesIn
            "open System.Linq\ntype Db() =\n    member _.People = [ 1; 2 ].AsQueryable()\nlet f (db: Db) (xs: int list) =\n    for x in xs do\n        for p in db.People do\n            printfn \"%d %d\" x p"

    match suggestions with
    | [ s ] -> Assert.Equal("db.People", s.SourceText)
    | other -> failwithf "Expected exactly one N+1 note, got %A" other

[<Fact>]
let ``local queryable value is also noted`` () =
    let suggestions =
        queriesIn
            "open System.Linq\nlet q = [ 1; 2 ].AsQueryable()\nlet f (xs: int list) =\n    for x in xs do\n        for p in q do\n            printfn \"%d %d\" x p"

    match suggestions with
    | [ s ] -> Assert.Equal("q", s.SourceText)
    | other -> failwithf "Expected exactly one local-queryable note, got %A" other

[<Fact>]
let ``in-memory inner sequence is fine`` () =
    Assert.Empty(
        queriesIn
            "let ys = [ 1; 2 ]\nlet f (xs: int list) =\n    for x in xs do\n        for y in ys do\n            printfn \"%d %d\" x y"
    )

[<Fact>]
let ``single un-nested queryable loop is fine`` () =
    Assert.Empty(
        queriesIn
            "open System.Linq\nlet q = [ 1; 2 ].AsQueryable()\nlet f () =\n    for p in q do\n        printfn \"%d\" p"
    )

[<Fact>]
let ``chunkBySize batching suppresses the note`` () =
    Assert.Empty(
        queriesIn
            "open System.Linq\nlet q = [ 1; 2 ].AsQueryable()\nlet f (xs: int list) =\n    for chunk in xs |> List.chunkBySize 100 do\n        for p in q do\n            printfn \"%d %d\" (List.sum chunk) p"
    )

[<Fact>]
let ``while loop around a queryable is also noted`` () =
    let suggestions =
        queriesIn
            "open System.Linq\nlet q = [ 1; 2 ].AsQueryable()\nlet f () =\n    let mutable go = true\n    while go do\n        for p in q do\n            printfn \"%d\" p\n        go <- false"

    match suggestions with
    | [ s ] -> Assert.Equal("q", s.SourceText)
    | other -> failwithf "Expected exactly one while-nested note, got %A" other
