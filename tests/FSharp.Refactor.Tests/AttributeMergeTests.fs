module FSharp.Refactor.Tests.AttributeMergeTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private mergesIn (source: string) =
    let tree, sourceText = parse source
    AttributeMerge.find tree sourceText

let private assertMerge (source: string) (expectedPatched: string) =
    match mergesIn source with
    | [ s ] ->
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.Equal(expectedPatched, patched)
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one merge suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``stacked attribute lines merge into one bracket`` () =
    assertMerge
        "module Test\n[<System.Obsolete>]\n[<CompiledName(\"Run\")>]\nlet f () = 1"
        "module Test\n[<System.Obsolete; CompiledName(\"Run\")>]\nlet f () = 1"

[<Fact>]
let ``same-line attribute brackets merge too`` () =
    assertMerge
        "module Test\n[<System.Obsolete>] [<CompiledName(\"Run\")>]\nlet f () = 1"
        "module Test\n[<System.Obsolete; CompiledName(\"Run\")>]\nlet f () = 1"

[<Fact>]
let ``type attributes merge`` () =
    assertMerge
        "module Test\n[<Sealed>]\n[<AllowNullLiteral>]\ntype C() =\n    member _.X = 1"
        "module Test\n[<Sealed; AllowNullLiteral>]\ntype C() =\n    member _.X = 1"

[<Fact>]
let ``a single attribute list is fine`` () =
    Assert.Empty(mergesIn "module Test\n[<System.Obsolete; CompiledName(\"Run\")>]\nlet f () = 1")

[<Fact>]
let ``a comment between brackets suppresses the merge`` () =
    Assert.Empty(mergesIn "module Test\n[<System.Obsolete>]\n// keep separate\n[<CompiledName(\"Run\")>]\nlet f () = 1")
