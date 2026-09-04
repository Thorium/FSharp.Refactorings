module FSharp.Refactor.Tests.RecordFieldsTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

// ---- FR0145 RecordFields ----

let private findIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    RecordFields.find tree sourceText checkResults

[<Literal>]
let private config =
    "module Test\ntype Config = { Name: string; Retries: int; Tags: string list; Timeout: int option; Owners: Set<string> }\n"

[<Fact>]
let ``obvious empties are added inline and the result typechecks`` () =
    let source = config + "let c = { Name = \"x\"; Retries = 3 }"

    match findIn source with
    | [ s ] ->
        Assert.Equal("; Tags = []; Timeout = None; Owners = Set.empty", s.InsertText)
        Assert.True s.AllObvious
        Assert.Equal<string list>([ "Tags"; "Timeout"; "Owners" ], s.Missing)
        let patched = applyEdit source s.Range s.InsertText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one suggestion, got %A" other

[<Fact>]
let ``a multi-line record gets one field per line at the label column`` () =
    let source = config + "let c =\n    { Name = \"x\"\n      Retries = 3 }"

    match findIn source with
    | [ s ] ->
        Assert.Equal("\n      Tags = []\n      Timeout = None\n      Owners = Set.empty", s.InsertText)
        let patched = applyEdit source s.Range s.InsertText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one suggestion, got %A" other

[<Fact>]
let ``a field with no obvious default gets a placeholder, and a zero alternative`` () =
    let source =
        config
        + "let c = { Name = \"x\"; Tags = []; Timeout = None; Owners = Set.empty }"

    match findIn source with
    | [ s ] ->
        Assert.False s.AllObvious
        Assert.Equal("; Retries = raise (System.NotImplementedException \"Retries\")", s.InsertText)
        Assert.Equal("; Retries = 0", s.ZeroInsertText)
        let patched = applyEdit source s.Range s.InsertText
        Assert.True(typechecksCleanly patched, $"Placeholder form does not typecheck:\n%s{patched}")
        let zeroed = applyEdit source s.Range s.ZeroInsertText
        Assert.True(typechecksCleanly zeroed, $"Zero form does not typecheck:\n%s{zeroed}")
    | other -> failwithf "Expected one suggestion, got %A" other

[<Fact>]
let ``a reference-typed field zeroes to Unchecked.defaultof`` () =
    let source =
        "module Test\ntype Inner = { V: int }\ntype Outer = { Label: string; Inner: Inner }\nlet o = { Label = \"x\" }"

    match findIn source with
    | [ s ] -> Assert.Equal("; Inner = Unchecked.defaultof<_>", s.ZeroInsertText)
    | other -> failwithf "Expected one suggestion, got %A" other

[<Fact>]
let ``a copy-and-update and a complete record are left alone`` () =
    Assert.Empty(
        findIn (
            config
            + "let a = { Name = \"x\"; Retries = 3; Tags = []; Timeout = None; Owners = Set.empty }\nlet b = { a with Retries = 4 }"
        )
    )

[<Fact>]
let ``a Guid field zeroes to Guid.Empty`` () =
    let source =
        "module Test\ntype Row = { Label: string; Id: System.Guid }\nlet r = { Label = \"x\" }"

    match findIn source with
    | [ s ] ->
        Assert.Equal("; Id = System.Guid.Empty", s.ZeroInsertText)
        let zeroed = applyEdit source s.Range s.ZeroInsertText
        Assert.True(typechecksCleanly zeroed, $"Zero form does not typecheck:\n%s{zeroed}")
    | other -> failwithf "Expected one suggestion, got %A" other
