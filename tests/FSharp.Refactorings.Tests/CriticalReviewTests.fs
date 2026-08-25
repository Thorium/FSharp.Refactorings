/// Regression tests from the 2026-08-26 critical review: replacement
/// logic errors and false-positive identifications, one test per
/// confirmed finding.
module FSharp.Refactorings.Tests.CriticalReviewTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

[<Fact>]
let ``FR0026: a field assigned in another member is not an auto-property`` () : unit =
    // the assignment is a LongIdentSet, not an Ident expression — the fix
    // used to delete the field and leave Reset() referencing nothing
    let tree, sourceText =
        parse
            "module Test\ntype Person() =\n    let mutable name = \"\"\n    member _.Reset() = name <- \"\"\n    member this.Name\n        with get () = name\n        and set v = name <- v"

    Assert.Empty(AutoProperty.find tree sourceText)

[<Fact>]
let ``FR0034: a shadowing lambda parameter suppresses the rewrite`` () : unit =
    // the inner x is a different option; substituting its .Value with the
    // outer binder would change which value the code reads
    let tree, sourceText, check =
        parseAndCheck
            "let f (x: int option) = if x.IsSome then [ Some 1 ] |> List.map (fun x -> x.Value) |> List.sum else 0"

    Assert.Empty(OptionMatch.find tree sourceText check)

[<Fact>]
let ``FR0031: a shadowed plus operator is never rewritten`` () : unit =
    let tree, sourceText, check =
        parseAndCheck "let (+) (a: string) (b: string) = a\nlet f (x: string) = \"pre \" + x + \"!\""

    Assert.Empty(StringConcat.find tree sourceText check)

[<Fact>]
let ``FR0035: a per-iteration collection is not loop-invariant`` () : unit =
    let tree, sourceText =
        parse
            "module Test\nlet f (xs: int list) =\n    for x in xs do\n        let ys = [ x; x + 1 ]\n        if List.contains x ys then printfn \"%d\" x"

    let contains, _ = LoopPerf.find tree sourceText
    Assert.Empty contains

[<Fact>]
let ``FR0021: ToString under a typed hole pins the type and stays`` () : unit =
    // %s requires a string; dropping .ToString() would not typecheck
    let tree, sourceText = parse "module Test\nlet f (x: int) = $\"%s{x.ToString()}\""
    Assert.Empty(InterpToString.find tree sourceText)

[<Fact>]
let ``FR0021: ToString in an untyped hole is still simplified`` () : unit =
    let tree, sourceText =
        parse "module Test\nlet f (x: int) = $\"{x.ToString()} items\""

    match InterpToString.find tree sourceText with
    | [ s ] -> Assert.Equal("x", s.ReplacementText)
    | other -> failwithf "Expected exactly one ToString suggestion, got %A" other

[<Fact>]
let ``FR0025: a shadowed isNull is never rewritten`` () : unit =
    let tree, sourceText, check =
        parseAndCheck "let isNull (s: string) = s.Length = 0\nlet f (s: string) = if isNull s then None else Some s"

    Assert.Empty(OptionOfObj.find tree sourceText check)
