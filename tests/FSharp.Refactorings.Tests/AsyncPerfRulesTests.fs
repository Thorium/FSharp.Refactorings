module FSharp.Refactorings.Tests.AsyncPerfRulesTests

open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

// ---- FR0049 SyncOverAsync ----

let private blockingIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    SyncOverAsync.find tree sourceText checkResults

[<Fact>]
let ``Result inside a task is flagged`` () =
    match blockingIn "let f (t: System.Threading.Tasks.Task<int>) = task { return t.Result + 1 }" with
    | [ s ] ->
        Assert.Equal(SyncOverAsync.BlockKind.TaskResult, s.Kind)
        Assert.Equal(Some "task", s.Builder)
    | other -> failwithf "Expected exactly one Result site, got %A" other

[<Fact>]
let ``Wait inside an async is flagged`` () =
    match blockingIn "let f (t: System.Threading.Tasks.Task) = async { t.Wait() }" with
    | [ s ] -> Assert.Equal(SyncOverAsync.BlockKind.TaskWait, s.Kind)
    | other -> failwithf "Expected exactly one Wait site, got %A" other

[<Fact>]
let ``GetResult outside any CE is still an antipattern`` () =
    match blockingIn "let f (t: System.Threading.Tasks.Task<int>) = t.GetAwaiter().GetResult()" with
    | [ s ] ->
        Assert.Equal(SyncOverAsync.BlockKind.AwaiterGetResult, s.Kind)
        Assert.Equal(None, s.Builder)
    | other -> failwithf "Expected exactly one boundary GetResult site, got %A" other

[<Fact>]
let ``RunSynchronously inside a task is flagged`` () =
    match blockingIn "let f (comp: Async<int>) = task { return (comp |> Async.RunSynchronously) }" with
    | [ s ] -> Assert.Equal(SyncOverAsync.BlockKind.RunSynchronously, s.Kind)
    | other -> failwithf "Expected exactly one RunSynchronously site, got %A" other

[<Fact>]
let ``Thread Sleep in an async gets the Async Sleep fix`` () =
    match blockingIn "let f () = async {\n    System.Threading.Thread.Sleep 100\n    return 1\n}" with
    | [ s ] ->
        match s.Fix with
        | Some(r, _, replacement) ->
            Assert.Equal("do! Async.Sleep 100", replacement)

            let source =
                "let f () = async {\n    System.Threading.Thread.Sleep 100\n    return 1\n}"

            let patched = applyEdit source r replacement
            Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
        | None -> failwith "Expected a fix for Thread.Sleep"
    | other -> failwithf "Expected exactly one Sleep site, got %A" other

[<Fact>]
let ``Thread Sleep in sync code is legitimate`` () =
    Assert.Empty(blockingIn "let f () = System.Threading.Thread.Sleep 100")

[<Fact>]
let ``a user type with a Result property is not a task`` () =
    Assert.Empty(blockingIn "type R() =\n    member _.Result = 1\nlet f (r: R) = task { return r.Result }")

[<Fact>]
let ``binding with let-bang is fine`` () =
    Assert.Empty(blockingIn "let f (t: System.Threading.Tasks.Task<int>) = task {\n    let! x = t\n    return x\n}")

// ---- FR0050 / FR0051 Accumulation ----

let private accumulationIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    Accumulation.find tree sourceText checkResults

let private assertFold (source: string) (expectedReplacement: string) =
    match accumulationIn source with
    | [ s ], _ ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, sprintf "Patched source does not typecheck:\n%s" patched)
    | other -> failwithf "Expected exactly one fold suggestion, got %A" other

[<Fact>]
let ``sum accumulation becomes Seq sum`` () =
    assertFold
        "let f (xs: int list) =\n    let mutable total = 0\n    for x in xs do\n        total <- total + x\n    total * 2"
        "let total = xs |> Seq.sum"

[<Fact>]
let ``projected sum becomes sumBy`` () =
    assertFold
        "let f (xs: int list) =\n    let mutable total = 0\n    for x in xs do\n        total <- total + x * x\n    total"
        "let total = xs |> Seq.sumBy (fun x -> x * x)"

[<Fact>]
let ``general combine becomes a fold`` () =
    assertFold
        "let f (xs: int list) =\n    let mutable best = 1\n    for x in xs do\n        best <- max best (x % 7)\n    best"
        "let best = xs |> Seq.fold (fun best x -> max best (x % 7)) 1"

[<Fact>]
let ``reassignment after the loop keeps the mutable`` () =
    let folds, _ =
        accumulationIn
            "let f (xs: int list) =\n    let mutable total = 0\n    for x in xs do\n        total <- total + x\n    total <- total + 1\n    total"

    Assert.Empty folds

[<Fact>]
let ``quadratic list append in a loop is noted`` () =
    let _, quadratics =
        accumulationIn
            "let f (xs: int list) =\n    let mutable acc: int list = []\n    for x in xs do\n        if x > 0 then acc <- acc @ [ x ]\n    acc"

    match quadratics with
    | [ s ] -> Assert.Equal("acc", s.Name)
    | other -> failwithf "Expected exactly one quadratic note, got %A" other

[<Fact>]
let ``quadratic Array append in a loop is noted`` () =
    let _, quadratics =
        accumulationIn
            "let f (xs: int list) =\n    let mutable acc: int[] = [||]\n    for x in xs do\n        if x > 0 then acc <- Array.append acc [| x |]\n    acc"

    match quadratics with
    | [ s ] -> Assert.Equal("acc", s.Name)
    | other -> failwithf "Expected exactly one Array-append note, got %A" other

// ---- FR0052 CountIsEmpty ----

let private countsIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    CountIsEmpty.find tree sourceText checkResults

[<Fact>]
let ``ConcurrentQueue Count equals zero becomes IsEmpty`` () =
    match countsIn "let f (q: System.Collections.Concurrent.ConcurrentQueue<int>) = q.Count = 0" with
    | [ s ] -> Assert.Equal("q.IsEmpty", s.ReplacementText)
    | other -> failwithf "Expected exactly one IsEmpty suggestion, got %A" other

[<Fact>]
let ``Count greater than zero negates IsEmpty`` () =
    match countsIn "let f (q: System.Collections.Concurrent.ConcurrentQueue<int>) = q.Count > 0" with
    | [ s ] -> Assert.Equal("not q.IsEmpty", s.ReplacementText)
    | other -> failwithf "Expected exactly one negated suggestion, got %A" other

[<Fact>]
let ``a List Count is a cheap field and fine`` () =
    Assert.Empty(countsIn "let f (xs: ResizeArray<int>) = xs.Count = 0")

// ---- FR0053 HexString ----

let private hexIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    HexString.find tree sourceText checkResults

[<Fact>]
let ``dash-stripped BitConverter chain becomes ToHexString`` () =
    match hexIn "module Test\nlet f (bytes: byte[]) = System.BitConverter.ToString(bytes).Replace(\"-\", \"\")" with
    | [ s ] -> Assert.Equal("System.Convert.ToHexString bytes", s.ReplacementText)
    | other -> failwithf "Expected exactly one hex suggestion, got %A" other

[<Fact>]
let ``other Replace arguments are left alone`` () =
    Assert.Empty(hexIn "module Test\nlet f (bytes: byte[]) = System.BitConverter.ToString(bytes).Replace(\"-\", \":\")")

// ---- FR0055 SwallowedException ----

let private swallowedIn (source: string) =
    let tree, sourceText = parse source
    SwallowedException.find tree sourceText

[<Fact>]
let ``empty wildcard catch is noted`` () =
    match swallowedIn "module Test\nlet f (act: unit -> unit) =\n    try act ()\n    with _ -> ()" with
    | [ s ] -> Assert.Equal("_", s.PatternText)
    | other -> failwithf "Expected exactly one swallow note, got %A" other

[<Fact>]
let ``empty typed Exception catch is noted`` () =
    match
        swallowedIn "module Test\nlet f (act: unit -> unit) =\n    try act ()\n    with :? System.Exception -> ()"
    with
    | [ _ ] -> ()
    | other -> failwithf "Expected exactly one typed swallow note, got %A" other

[<Fact>]
let ``handler that does something is fine`` () =
    Assert.Empty(
        swallowedIn "module Test\nlet f (act: unit -> unit) =\n    try act ()\n    with ex -> printfn \"%s\" ex.Message"
    )

[<Fact>]
let ``deliberately ignoring a specific exception is fine`` () =
    Assert.Empty(
        swallowedIn
            "module Test\nlet f (act: unit -> unit) =\n    try act ()\n    with :? System.OperationCanceledException -> ()"
    )

// ---- FR0054 RaiseInSpecialMember ----

let private objectRulesIn (source: string) =
    let tree, sourceText = parse source
    ObjectRules.find tree sourceText

[<Fact>]
let ``failwith inside GetHashCode is noted`` () =
    let _, _, raises =
        objectRulesIn
            "module Test\ntype T() =\n    override _.Equals(o) = false\n    override _.GetHashCode() = failwith \"no hash\""

    match raises with
    | [ s ] -> Assert.Equal("GetHashCode", s.MemberName)
    | other -> failwithf "Expected exactly one raise-in-special note, got %A" other

[<Fact>]
let ``ToString returning a value is fine`` () =
    let _, _, raises =
        objectRulesIn "module Test\ntype T() =\n    override _.ToString() = \"t\""

    Assert.Empty raises

// ---- FR0058 RecursiveSeq ----

let private recursiveSeqIn (source: string) =
    let tree, sourceText = parse source
    RecursiveSeq.find tree sourceText

[<Fact>]
let ``a rec function yielding itself through seq is noted`` () =
    match
        recursiveSeqIn
            "module Test\nlet rec countDown n = seq {\n    yield n\n    if n > 0 then yield! countDown (n - 1)\n}"
    with
    | [ s ] ->
        Assert.Equal("countDown", s.FunctionName)
        Assert.Equal("seq", s.Builder)
    | other -> failwithf "Expected exactly one recursive-seq note, got %A" other

[<Fact>]
let ``self-reference via Seq collect inside the seq is also caught`` () =
    match
        recursiveSeqIn
            "module Test\nlet rec walk (xs: int list list) = seq {\n    yield xs.Length\n    yield! Seq.collect walk []\n}"
    with
    | [ s ] -> Assert.Equal("walk", s.FunctionName)
    | other -> failwithf "Expected exactly one collect-form note, got %A" other

[<Fact>]
let ``a non-recursive seq is fine`` () =
    Assert.Empty(recursiveSeqIn "module Test\nlet numbers n = seq {\n    for i in 1..n do\n        yield i * i\n}")

[<Fact>]
let ``recursion outside any seq is fine`` () =
    Assert.Empty(
        recursiveSeqIn "module Test\nlet rec fact n = if n <= 1 then 1 else n * fact (n - 1)\nlet s = seq { yield 1 }"
    )

// ---- FR0057 XmlDocParams ----

let private xmlDocsIn (source: string) =
    let tree, sourceText = parse source
    XmlDocParams.find tree sourceText

[<Fact>]
let ``a missing param tag is noted`` () =
    match
        xmlDocsIn
            "module Test\n/// <summary>Scales.</summary>\n/// <param name=\"value\">The value.</param>\nlet scale (value: int) (factor: int) = value * factor"
    with
    | [ s ] ->
        Assert.Equal("scale", s.BindingName)
        Assert.Equal<string list>([ "factor" ], s.MissingParams)
    | other -> failwithf "Expected exactly one doc-drift note, got %A" other

[<Fact>]
let ``fully documented parameters are fine`` () =
    Assert.Empty(
        xmlDocsIn
            "module Test\n/// <summary>Scales.</summary>\n/// <param name=\"value\">The value.</param>\n/// <param name=\"factor\">The factor.</param>\nlet scale (value: int) (factor: int) = value * factor"
    )

[<Fact>]
let ``undocumented functions are a style choice`` () =
    Assert.Empty(xmlDocsIn "module Test\n/// Scales a value.\nlet scale (value: int) (factor: int) = value * factor")

[<Fact>]
let ``a custom operation documents the DSL keyword not the signature`` () =
    Assert.Empty(
        xmlDocsIn
            "module Test\ntype Cfg() =\n    member _.Yield(_: unit) = 0\n    /// <summary>Sets the width.</summary>\n    /// <param name=\"w\">The width.</param>\n    [<CustomOperation \"width\">]\n    member _.Width(state: int, w: int) = state + w"
    )

[<Fact>]
let ``sync-over-async inside an object expression member is found`` () =
    // corpus regression: Dispose bodies in `{ new IDisposable with ... }`
    // hid GetResult calls from the walker
    match
        blockingIn
            "module Test\nopen System\nopen System.Threading.Tasks\nlet scope (t: Task) =\n    { new IDisposable with\n        member _.Dispose() = t.GetAwaiter().GetResult() }"
    with
    | [ _ ] -> ()
    | other -> failwithf "Expected exactly one blocking note, got %A" other

[<Fact>]
let ``catch-all substituting an empty string is noted`` () =
    match swallowedIn "module Test\nlet f (read: unit -> string) =\n    try read ()\n    with _ -> \"\"" with
    | [ s ] -> Assert.Equal(Some "\"\"", s.FallbackText)
    | other -> failwithf "Expected exactly one fallback note, got %A" other

[<Fact>]
let ``catch-all substituting zero is noted`` () =
    match swallowedIn "module Test\nlet f (count: unit -> int) =\n    try count ()\n    with _ -> 0" with
    | [ s ] -> Assert.Equal(Some "0", s.FallbackText)
    | other -> failwithf "Expected exactly one zero-fallback note, got %A" other

[<Fact>]
let ``catch-all substituting defaultof is noted`` () =
    match
        swallowedIn
            "module Test\nlet f (get: unit -> string) =\n    try get ()\n    with _ -> Unchecked.defaultof<string>"
    with
    | [ _ ] -> ()
    | other -> failwithf "Expected exactly one defaultof-fallback note, got %A" other

[<Fact>]
let ``the bool probe idiom stays quiet`` () =
    // `try ping (); true with _ -> false` — the failure IS the answer
    Assert.Empty(
        swallowedIn
            "module Test\nlet canPing (ping: unit -> unit) =\n    try\n        ping ()\n        true\n    with _ -> false"
    )

[<Fact>]
let ``a specific exception with a default is a decision`` () =
    Assert.Empty(
        swallowedIn
            "module Test\nlet f (read: unit -> string) =\n    try read ()\n    with :? System.IO.FileNotFoundException -> \"\""
    )

[<Fact>]
let ``a string accumulator becomes String.concat, not a quadratic fold`` () =
    let folds, _ =
        accumulationIn
            "module Test\nlet joinAll (xs: string list) =\n    let mutable acc = \"\"\n    for x in xs do\n        acc <- acc + x\n    acc"

    match folds with
    | [ s ] -> Assert.Equal("let acc = xs |> String.concat \"\"", s.ReplacementText)
    | other -> failwithf "Expected exactly one string-concat fold, got %A" other

[<Fact>]
let ``a projected string accumulator maps then concats`` () =
    let folds, _ =
        accumulationIn
            "module Test\nlet render (xs: int list) =\n    let mutable acc = \"\"\n    for x in xs do\n        acc <- acc + string x\n    acc"

    match folds with
    | [ s ] -> Assert.Equal("let acc = xs |> Seq.map (fun x -> string x) |> String.concat \"\"", s.ReplacementText)
    | other -> failwithf "Expected exactly one mapped string concat, got %A" other

[<Fact>]
let ``a non-empty seed prefixes the concatenation`` () =
    let folds, _ =
        accumulationIn
            "module Test\nlet render (xs: string list) =\n    let mutable acc = \"head:\"\n    for x in xs do\n        acc <- acc + x\n    acc"

    match folds with
    | [ s ] -> Assert.Equal("let acc = \"head:\" + (xs |> String.concat \"\")", s.ReplacementText)
    | other -> failwithf "Expected exactly one seeded string concat, got %A" other
