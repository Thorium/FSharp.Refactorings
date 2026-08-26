module FSharp.Refactorings.Tests.QualityRulesTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

// ---- FR0061 ArgNames ----

let private argNamesIn (source: string) =
    let tree, sourceText = parse source
    ArgNames.find tree sourceText

[<Fact>]
let ``a misspelled invalidArg name is noted`` () =
    match
        argNamesIn
            "module Test\nlet scale (value: int) (factor: int) =\n    if factor = 0 then invalidArg \"facotr\" \"zero\"\n    value * factor"
    with
    | [ s ] ->
        Assert.Equal("facotr", s.UsedName)
        Assert.Equal<string list>([ "value"; "factor" ], s.ParameterNames)
    | other -> failwithf "Expected exactly one arg-name note, got %A" other

[<Fact>]
let ``a correct ArgumentNullException name is fine`` () =
    Assert.Empty(
        argNamesIn
            "module Test\nlet f (input: string) =\n    if isNull input then raise (System.ArgumentNullException \"input\")\n    input.Length"
    )

[<Fact>]
let ``ArgumentException second argument is the parameter name`` () =
    match
        argNamesIn
            "module Test\nlet f (input: string) =\n    if input = \"\" then raise (System.ArgumentException(\"empty\", \"inptu\"))\n    input.Length"
    with
    | [ s ] -> Assert.Equal("inptu", s.UsedName)
    | other -> failwithf "Expected exactly one ctor arg-name note, got %A" other

[<Fact>]
let ``a CLI flag name is not a parameter reference`` () =
    // "--mode" can never BE an F# parameter name; the author is naming an
    // external concept, not referencing the signature
    Assert.Empty(
        argNamesIn
            "module Test\nlet run (args: string list) =\n    if args.IsEmpty then invalidArg \"--mode\" \"missing\"\n    args.Length"
    )

[<Fact>]
let ``a custom operation validates its DSL operand name`` () =
    Assert.Empty(
        argNamesIn
            "module Test\ntype Cfg() =\n    member _.Yield(_: unit) = 0\n    [<CustomOperation \"vpc\">]\n    member _.Vpc(state: int) =\n        if state < 0 then invalidArg \"vpc\" \"bad\"\n        state"
    )

// ---- FR0063 / FR0064 ExceptionRules ----

let private exceptionsIn (source: string) =
    let tree, sourceText = parse source
    ExceptionRules.find tree sourceText

[<Fact>]
let ``raise inside finally is noted`` () =
    let finallies, _ =
        exceptionsIn
            "module Test\nlet f (act: unit -> int) (cleanup: unit -> unit) =\n    try\n        act ()\n    finally\n        cleanup ()\n        failwith \"cleanup failed\""

    Assert.Single finallies |> ignore

[<Fact>]
let ``a finally that only cleans up is fine`` () =
    let finallies, _ =
        exceptionsIn
            "module Test\nlet f (act: unit -> int) (cleanup: unit -> unit) =\n    try\n        act ()\n    finally\n        cleanup ()"

    Assert.Empty finallies

[<Fact>]
let ``raising a reserved runtime exception is noted`` () =
    let _, reserved =
        exceptionsIn "module Test\nlet f () = raise (System.IndexOutOfRangeException \"custom\")"

    match reserved with
    | [ s ] -> Assert.Equal("IndexOutOfRangeException", s.TypeName)
    | other -> failwithf "Expected exactly one reserved-exception note, got %A" other

[<Fact>]
let ``ordinary exceptions raise freely`` () =
    let _, reserved =
        exceptionsIn "module Test\nlet f () = raise (System.InvalidOperationException \"bad state\")"

    Assert.Empty reserved

// ---- FR0065 / FR0066 SecurityRules ----

let private securityIn (source: string) =
    let tree, sourceText = parse source
    SecurityRules.find tree sourceText

[<Fact>]
let ``MD5 Create is noted as weak`` () =
    let crypto, _ =
        securityIn "module Test\nlet hash () = System.Security.Cryptography.MD5.Create()"

    match crypto with
    | [ s ] -> Assert.Equal(SecurityRules.WeakKind.Hash "MD5", s.Kind)
    | other -> failwithf "Expected exactly one weak-hash note, got %A" other

[<Fact>]
let ``SHA256 is fine`` () =
    let crypto, _ =
        securityIn "module Test\nlet hash () = System.Security.Cryptography.SHA256.Create()"

    Assert.Empty crypto

[<Fact>]
let ``interpolated CommandText is noted`` () =
    let _, sql =
        securityIn
            "module Test\nlet q (cmd: obj) (setText: string -> unit) (name: string) =\n    setText $\"select * from users where name = '{name}'\""

    // setText is a function, not CommandText — nothing fires
    Assert.Empty sql

[<Fact>]
let ``CommandText assignment from interpolation is noted`` () =
    let _, sql =
        securityIn
            "module Test\ntype Cmd() =\n    member val CommandText = \"\" with get, set\nlet q (cmd: Cmd) (name: string) = cmd.CommandText <- $\"select * from t where n = '{name}'\""

    match sql with
    | [ s ] -> Assert.Equal("CommandText", s.Sink)
    | other -> failwithf "Expected exactly one sql note, got %A" other

[<Fact>]
let ``a literal CommandText is fine`` () =
    let _, sql =
        securityIn
            "module Test\ntype Cmd() =\n    member val CommandText = \"\" with get, set\nlet q (cmd: Cmd) = cmd.CommandText <- \"select * from t where n = @n\""

    Assert.Empty sql

// ---- FR0062 / FR0067 / FR0068 MiscRules ----

let private miscIn (source: string) =
    let tree, sourceText = parse source
    MiscRules.find tree sourceText

[<Fact>]
let ``public module-level mutable is noted`` () =
    let mutables, _, _ =
        miscIn "module Test\nlet mutable counter = 0\nlet bump () = counter <- counter + 1"

    match mutables with
    | [ s ] -> Assert.Equal("counter", s.Name)
    | other -> failwithf "Expected exactly one mutable-state note, got %A" other

[<Fact>]
let ``private module-level mutable is a contained decision`` () =
    let mutables, _, _ =
        miscIn "module Test\nlet mutable private counter = 0\nlet bump () = counter <- counter + 1"

    Assert.Empty mutables

[<Fact>]
let ``a mutable inside a private module is confined`` () =
    let mutables, _, _ =
        miscIn "module Test\nmodule private State =\n    let mutable counter = 0"

    Assert.Empty mutables

[<Fact>]
let ``cultureless DateTime Parse is noted`` () =
    let _, parses, _ = miscIn "module Test\nlet f (s: string) = System.DateTime.Parse s"

    match parses with
    | [ s ] -> Assert.Equal("DateTime.Parse", s.CallName)
    | other -> failwithf "Expected exactly one culture note, got %A" other

[<Fact>]
let ``Parse with a culture argument is fine`` () =
    let _, parses, _ =
        miscIn
            "module Test\nlet f (s: string) = System.DateTime.Parse(s, System.Globalization.CultureInfo.InvariantCulture)"

    Assert.Empty parses

[<Fact>]
let ``int Parse is low-risk and not flagged`` () =
    let _, parses, _ = miscIn "module Test\nlet f (s: string) = System.Int32.Parse s"
    Assert.Empty parses

[<Fact>]
let ``duplicate enum values are noted`` () =
    let _, _, enums =
        miscIn "module Test\ntype Color =\n    | Red = 1\n    | Green = 2\n    | Crimson = 1"

    match enums with
    | [ s ] ->
        Assert.Equal("Crimson", s.CaseName)
        Assert.Equal("Red", s.OriginalName)
    | other -> failwithf "Expected exactly one duplicate-enum note, got %A" other

[<Fact>]
let ``distinct enum values are fine`` () =
    let _, _, enums = miscIn "module Test\ntype Color =\n    | Red = 1\n    | Green = 2"
    Assert.Empty enums

[<Fact>]
let ``a builder Run member validates DSL keywords not parameters`` () =
    Assert.Empty(
        argNamesIn
            "module Test\ntype SgBuilder() =\n    member _.Yield(_: unit) = 0\n    member _.Zero() = 0\n    member _.Run(config: int) =\n        if config < 0 then invalidArg \"vpc\" \"required\"\n        config"
    )

// ---- FR0069 / FR0070 StructHints ----

let private structHintsIn (source: string) =
    let tree, sourceText = parse source
    StructHints.find false tree sourceText

/// The same scan with API changes allowed, as `fsharp-refactor
/// --api-changes` runs it.
let private structHintsWithApiChangesIn (source: string) =
    let tree, sourceText = parse source
    StructHints.find true tree sourceText

[<Fact>]
let ``a public record is offered under api changes`` () =
    let voptions, _, _ =
        structHintsWithApiChangesIn "module Test\ntype Row = { Id: System.Guid option; Name: string }"

    Assert.NotEmpty voptions

[<Fact>]
let ``a struct option field in a private record suggests voption`` () =
    let voptions, _, _ =
        structHintsIn "module Test\ntype private Row = { Id: System.Guid option; Name: string }"

    match voptions with
    | [ s ] ->
        Assert.Equal("Id", s.FieldName)
        Assert.Equal("System.Guid", s.ElementText)
    | other -> failwithf "Expected exactly one voption note, got %A" other

[<Fact>]
let ``a public record keeps its option fields`` () =
    // serialization shapes and call sites are unbounded for public types
    let voptions, _, _ = structHintsIn "module Test\ntype Row = { Id: int option }"
    Assert.Empty voptions

[<Fact>]
let ``a reference payload keeps its option`` () =
    let voptions, _, _ =
        structHintsIn "module Test\ntype private Row = { Name: string option }"

    Assert.Empty voptions

[<Fact>]
let ``a record in a private module is contained`` () =
    let voptions, _, _ =
        structHintsIn "module Test\nmodule private Inner =\n    type Row = { Stamp: System.DateTime option }"

    Assert.Single voptions |> ignore

[<Fact>]
let ``a small all-struct private record suggests Struct`` () =
    let _, structs, _ =
        structHintsIn "module Test\ntype private Point = { X: float; Y: float }"

    match structs with
    | [ s ] ->
        Assert.Equal("Point", s.TypeName)
        Assert.Equal(2, s.FieldCount)
    | other -> failwithf "Expected exactly one struct note, got %A" other

[<Fact>]
let ``five fields are past the struct sweet spot`` () =
    let _, structs, _ =
        structHintsIn "module Test\ntype private Wide = { A: int; B: int; C: int; D: int; E: int }"

    Assert.Empty structs

[<Fact>]
let ``a string field keeps the record on the heap`` () =
    let _, structs, _ =
        structHintsIn "module Test\ntype private Named = { X: int; Name: string }"

    Assert.Empty structs

[<Fact>]
let ``an existing Struct attribute is already done`` () =
    let _, structs, _ =
        structHintsIn "module Test\n[<Struct>]\ntype private Point = { X: float; Y: float }"

    Assert.Empty structs

[<Fact>]
let ``a mutable field would change copy semantics`` () =
    let _, structs, _ =
        structHintsIn "module Test\ntype private Counter = { mutable N: int }"

    Assert.Empty structs

[<Fact>]
let ``a voption field already counts as flat for the struct hint`` () =
    let _, structs, _ =
        structHintsIn "module Test\ntype private Row = { Id: int voption; N: int }"

    Assert.Single structs |> ignore

// ---- FR0071 LoopInvariant ----

let private invariantsIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    LoopInvariant.find tree sourceText checkResults

let private assertHoisted (source: string) (expectedPatched: string) =
    match invariantsIn source with
    | [ s ] ->
        let patched =
            s.Edits
            |> List.sortByDescending (fun (r, _, _) -> r.StartLine, r.StartColumn)
            |> List.fold (fun acc (r, _, replacement) -> applyEdit acc r replacement) source

        Assert.Equal(expectedPatched, patched)
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one invariant note, got %A" other

[<Fact>]
let ``an invariant binding hoists out of a for loop`` () =
    assertHoisted
        "let sink (n: int) = ()\nlet run (a: int) =\n    for x = 0 to 100 do\n        let c = a + 3\n        sink (x + c)"
        "let sink (n: int) = ()\nlet run (a: int) =\n    let c = a + 3\n    for x = 0 to 100 do\n        sink (x + c)"

[<Fact>]
let ``an invariant binding hoists out of a foreach loop`` () =
    assertHoisted
        "let sink (n: int) = ()\nlet run (a: int) (xs: int list) =\n    for x in xs do\n        let c = a + 3\n        sink (x + c)"
        "let sink (n: int) = ()\nlet run (a: int) (xs: int list) =\n    let c = a + 3\n    for x in xs do\n        sink (x + c)"

[<Fact>]
let ``an invariant binding hoists out of a map lambda`` () =
    assertHoisted
        "let run (a: int) (xs: int list) =\n    xs\n    |> List.map (fun x ->\n        let c = a + 3\n        x + c)"
        "let run (a: int) (xs: int list) =\n    let c = a + 3\n    xs\n    |> List.map (fun x ->\n        x + c)"

[<Fact>]
let ``a loop-variable-dependent binding stays`` () =
    Assert.Empty(
        invariantsIn
            "let sink (n: int) = ()\nlet run (a: int) =\n    for x = 0 to 100 do\n        let c = a + x\n        sink (x + c)"
    )

[<Fact>]
let ``a function-call binding is not provably pure and stays`` () =
    Assert.Empty(
        invariantsIn
            "let compute (n: int) = n * 2\nlet sink (n: int) = ()\nlet run (a: int) =\n    for x = 0 to 100 do\n        let c = compute a\n        sink (x + c)"
    )

[<Fact>]
let ``a binding reading a variable the loop mutates stays`` () =
    Assert.Empty(
        invariantsIn
            "let sink (n: int) = ()\nlet run (a: int) =\n    let mutable m = a\n    for x = 0 to 100 do\n        let c = m + 3\n        m <- m + 1\n        sink (x + c)"
    )

[<Fact>]
let ``a name used after the loop stays put`` () =
    // hoisting would widen the binding's scope over the later use
    Assert.Empty(
        invariantsIn
            "let sink (n: int) = ()\nlet run (a: int) =\n    let c = 99\n    for x = 0 to 100 do\n        let c = a + 3\n        sink (x + c)\n    sink c"
    )

[<Fact>]
let ``while loops hoist too`` () =
    assertHoisted
        "let sink (n: int) = ()\nlet run (a: int) (keep: unit -> bool) =\n    while keep () do\n        let c = a + 3\n        sink c"
        "let sink (n: int) = ()\nlet run (a: int) (keep: unit -> bool) =\n    let c = a + 3\n    while keep () do\n        sink c"

// ---- FR0072 ExpandWildcard ----

let private wildcardsIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    ExpandWildcard.find tree sourceText checkResults

let private assertExpanded (source: string) (expectedReplacement: string) =
    match wildcardsIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one wildcard note, got %A" other

[<Fact>]
let ``a wildcard hiding one case expands to it`` () =
    assertExpanded
        "type T =\n    | A\n    | B\n    | C\n    | D\n\nlet f (t: T) =\n    match t with\n    | A -> 1\n    | B -> 2\n    | C -> 3\n    | _ -> 4"
        "D"

[<Fact>]
let ``a wildcard hiding two cases expands to an or-pattern`` () =
    assertExpanded
        "type T =\n    | A\n    | B\n    | C\n    | D\n\nlet f (t: T) =\n    match t with\n    | A -> 1\n    | B -> 2\n    | _ -> 0"
        "C | D"

[<Fact>]
let ``a hidden case with fields gets a payload wildcard`` () =
    assertExpanded
        "type T =\n    | A\n    | B of int\n\nlet f (t: T) =\n    match t with\n    | A -> 1\n    | _ -> 0"
        "B _"

[<Fact>]
let ``three hidden cases would bloat the match and stay`` () =
    Assert.Empty(
        wildcardsIn
            "type T =\n    | A\n    | B\n    | C\n    | D\n\nlet f (t: T) =\n    match t with\n    | A -> 1\n    | _ -> 0"
    )

[<Fact>]
let ``a guarded clause breaks coverage reasoning`` () =
    Assert.Empty(
        wildcardsIn
            "type T =\n    | A of int\n    | B\n\nlet f (t: T) =\n    match t with\n    | A n when n > 0 -> 1\n    | _ -> 0"
    )

[<Fact>]
let ``a literal payload covers its case only partially`` () =
    Assert.Empty(
        wildcardsIn
            "type T =\n    | A of int\n    | B\n\nlet f (t: T) =\n    match t with\n    | A 3 -> 1\n    | _ -> 0"
    )

[<Fact>]
let ``enums are open sets and stay wild`` () =
    Assert.Empty(
        wildcardsIn
            "type Color =\n    | Red = 1\n    | Green = 2\n\nlet f (c: Color) =\n    match c with\n    | Color.Red -> 1\n    | _ -> 0"
    )

[<Fact>]
let ``an option match expands to None`` () =
    assertExpanded "let f (x: int option) =\n    match x with\n    | Some v -> v\n    | _ -> 0" "None"

[<Fact>]
let ``a RequireQualifiedAccess union keeps its qualifier in the fix`` () =
    assertExpanded
        "[<RequireQualifiedAccess>]\ntype Mode =\n    | Fast\n    | Careful\n    | Dry\n\nlet f (m: Mode) =\n    match m with\n    | Mode.Fast -> 1\n    | Mode.Careful -> 2\n    | _ -> 0"
        "Mode.Dry"

// ---- FR0093 StructTupleField ----

let private structTuplesIn (source: string) =
    let _, _, tuples = structHintsIn source
    tuples

[<Fact>]
let ``a struct tuple field in a private record is noted`` () =
    match structTuplesIn "module Test\ntype private Row = { Span: int * int }" with
    | [ s ] ->
        Assert.Equal("Span", s.FieldName)
        Assert.Equal("int * int", s.TupleText)
        Assert.Equal("Row", s.TypeName)
    | other -> failwithf "Expected exactly one struct-tuple note, got %A" other

[<Fact>]
let ``four elements are still worth flattening`` () =
    Assert.Single(structTuplesIn "module Test\ntype private Box = { Bounds: float * float * float * float }")
    |> ignore

[<Fact>]
let ``five elements copy more than they save`` () =
    Assert.Empty(structTuplesIn "module Test\ntype private Wide = { P: int * int * int * int * int }")

[<Fact>]
let ``a reference element keeps the tuple on the heap`` () =
    Assert.Empty(structTuplesIn "module Test\ntype private Row = { Pair: int * string }")

[<Fact>]
let ``an already struct tuple is left alone`` () =
    Assert.Empty(structTuplesIn "module Test\ntype private Row = { Span: struct (int * int) }")

[<Fact>]
let ``a public record keeps its reference tuples`` () =
    Assert.Empty(structTuplesIn "module Test\ntype Row = { Span: int * int }")

[<Fact>]
let ``a public record tuple is offered under api changes`` () =
    let _, _, tuples =
        structHintsWithApiChangesIn "module Test\ntype Row = { Span: int * int }"

    Assert.Single tuples |> ignore

[<Fact>]
let ``a plain struct field is not a tuple`` () =
    Assert.Empty(structTuplesIn "module Test\ntype private Row = { Count: int }")
