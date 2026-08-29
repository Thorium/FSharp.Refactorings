module FSharp.Refactor.Tests.DuFieldNamesTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private fieldNamesIn (source: string) =
    let tree, sourceText = parse source
    DuFieldNames.find false tree sourceText

/// The same scan with API changes allowed, as `fsharp-refactor
/// --api-changes` runs it.
let private fieldNamesWithApiChangesIn (source: string) =
    let tree, sourceText = parse source
    DuFieldNames.find true tree sourceText

/// Apply a suggestion's edits bottom-up and verify the patched text.
let private assertFieldNames (source: string) (expectedPatched: string) =
    match fieldNamesIn source with
    | [ s ] ->
        let patched =
            s.Edits
            |> List.sortByDescending (fun (r, _, _) -> r.StartLine, r.StartColumn)
            |> List.fold (fun acc (r, _, t) -> applyEdit acc r t) source

        Assert.Equal(expectedPatched, patched)
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one field-name suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``match-site names flow onto the private case definition`` () =
    assertFieldNames
        "module Test\ntype private Order =\n    | Line of int * decimal\n    | Total of decimal\nlet private f (o: Order) =\n    match o with\n    | Line(qty, price) -> decimal qty * price\n    | Total t -> t"
        "module Test\ntype private Order =\n    | Line of qty: int * price: decimal\n    | Total of decimal\nlet private f (o: Order) =\n    match o with\n    | Line(qty, price) -> decimal qty * price\n    | Total t -> t"

[<Fact>]
let ``representation-private union is also accepted`` () =
    assertFieldNames
        "module Test\ntype Order =\n    private\n    | Line of int * decimal\n    | Total of decimal\nlet f (o: Order) =\n    match o with\n    | Line(qty, price) -> decimal qty * price\n    | Total t -> t"
        "module Test\ntype Order =\n    private\n    | Line of qty: int * price: decimal\n    | Total of decimal\nlet f (o: Order) =\n    match o with\n    | Line(qty, price) -> decimal qty * price\n    | Total t -> t"

[<Fact>]
let ``wildcard sites do not block the harvest`` () =
    assertFieldNames
        "module Test\ntype private Pair =\n    | Pair of int * int\n    | Empty\nlet private f p =\n    match p with\n    | Pair(first, second) -> first + second\n    | Empty -> 0\nlet private g p =\n    match p with\n    | Pair _ -> true\n    | Empty -> false"
        "module Test\ntype private Pair =\n    | Pair of first: int * second: int\n    | Empty\nlet private f p =\n    match p with\n    | Pair(first, second) -> first + second\n    | Empty -> 0\nlet private g p =\n    match p with\n    | Pair _ -> true\n    | Empty -> false"

[<Fact>]
let ``type inside an internal module is accepted`` () =
    assertFieldNames
        "module Test\nmodule internal Impl =\n    type OrderLine =\n        | Line of int * decimal\n        | Total of decimal\n    let f (o: OrderLine) =\n        match o with\n        | Line(qty, price) -> decimal qty * price\n        | Total t -> t"
        "module Test\nmodule internal Impl =\n    type OrderLine =\n        | Line of qty: int * price: decimal\n        | Total of decimal\n    let f (o: OrderLine) =\n        match o with\n        | Line(qty, price) -> decimal qty * price\n        | Total t -> t"

[<Fact>]
let ``internal top-level module is accepted`` () =
    assertFieldNames
        "module internal Test\ntype OrderLine =\n    | Line of int * decimal\n    | Total of decimal\nlet f (o: OrderLine) =\n    match o with\n    | Line(qty, price) -> decimal qty * price\n    | Total t -> t"
        "module internal Test\ntype OrderLine =\n    | Line of qty: int * price: decimal\n    | Total of decimal\nlet f (o: OrderLine) =\n    match o with\n    | Line(qty, price) -> decimal qty * price\n    | Total t -> t"

[<Fact>]
let ``public type is left alone`` () =
    Assert.Empty(
        fieldNamesIn
            "module Test\ntype Order =\n    | Line of int * decimal\nlet f (o: Order) =\n    match o with\n    | Line(qty, price) -> decimal qty * price"
    )

[<Fact>]
let ``public type is offered under api changes`` () =
    Assert.NotEmpty(
        fieldNamesWithApiChangesIn
            "module Test\ntype Order =\n    | Line of int * decimal\nlet f (o: Order) =\n    match o with\n    | Line(qty, price) -> decimal qty * price"
    )

[<Fact>]
let ``disagreeing sites cancel the suggestion`` () =
    Assert.Empty(
        fieldNamesIn
            "module Test\ntype private Order =\n    | Line of int * decimal\nlet f (o: Order) =\n    match o with\n    | Line(qty, price) -> decimal qty * price\nlet g (o: Order) =\n    match o with\n    | Line(n, total) -> decimal n * total"
    )

[<Fact>]
let ``partially named site cancels the suggestion`` () =
    Assert.Empty(
        fieldNamesIn
            "module Test\ntype private Order =\n    | Line of int * decimal\nlet f (o: Order) =\n    match o with\n    | Line(qty, _) -> qty"
    )

[<Fact>]
let ``already named fields are left alone`` () =
    Assert.Empty(
        fieldNamesIn
            "module Test\ntype private Order =\n    | Line of qty: int * price: decimal\nlet f (o: Order) =\n    match o with\n    | Line(qty, price) -> decimal qty * price"
    )

[<Fact>]
let ``ambiguous case name across two unions is skipped`` () =
    Assert.Empty(
        fieldNamesIn
            "module Test\ntype private A =\n    | Item of int * int\ntype private B =\n    | Item of string * string\nlet f (a: A) =\n    match a with\n    | Item(left, right) -> left + right"
    )

[<Fact>]
let ``case without any destructuring site is left alone`` () =
    Assert.Empty(fieldNamesIn "module Test\ntype private Order =\n    | Line of int * decimal\n    | Total of decimal")
