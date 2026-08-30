module FSharp.Refactor.Tests.AsyncPerfRulesTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

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
            Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
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
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one fold suggestion, got %A" other

[<Fact>]
let ``sum accumulation becomes Seq sum`` () =
    assertFold
        "let f (xs: int list) =\n    let mutable total = 0\n    for x in xs do\n        total <- total + x\n    total * 2"
        "let total = xs |> List.sum"

[<Fact>]
let ``projected sum becomes sumBy`` () =
    assertFold
        "let f (xs: int list) =\n    let mutable total = 0\n    for x in xs do\n        total <- total + x * x\n    total"
        "let total = xs |> List.sumBy (fun x -> x * x)"

[<Fact>]
let ``general combine becomes a fold`` () =
    assertFold
        "let f (xs: int list) =\n    let mutable best = 1\n    for x in xs do\n        best <- max best (x % 7)\n    best"
        "let best = xs |> List.fold (fun best x -> max best (x % 7)) 1"

[<Fact>]
let ``reassignment after the loop keeps the mutable`` () =
    let folds, _ =
        accumulationIn
            "let f (xs: int list) =\n    let mutable total = 0\n    for x in xs do\n        total <- total + x\n    total <- total + 1\n    total"

    Assert.Empty folds

[<Fact>]
let ``counting a non-generic IEnumerable stays a loop`` () =
    // from the corpus (SQLProvider SeqValues): `for` accepts the non-generic
    // IEnumerable, Seq.sumBy needs seq<'T> — the rewrite would be FS0001
    let folds, _ =
        accumulationIn
            "let f (values: System.Collections.IEnumerable) =\n    let mutable count = 0\n    for v in values do\n        count <- count + 1\n    count"

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

// ---- FR0107 FlagLoop ----

let private flagLoopsIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    Accumulation.findFlagLoops tree sourceText checkResults

let private assertFlagRewrite (source: string) (expectedReplacement: string) =
    match flagLoopsIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one flag-loop suggestion, got %A" other

[<Fact>]
let ``a false flag set in a loop becomes exists`` () =
    assertFlagRewrite
        "let f (xs: int list) =\n    let mutable found = false\n    for x in xs do\n        if x > 3 then found <- true\n    found"
        "let found = xs |> List.exists (fun x -> x > 3)"

[<Fact>]
let ``an array source resolves to Array exists`` () =
    assertFlagRewrite
        "let f (xs: int[]) =\n    let mutable found = false\n    for x in xs do\n        if x > 3 then found <- true\n    found"
        "let found = xs |> Array.exists (fun x -> x > 3)"

[<Fact>]
let ``a true flag falsified in a loop becomes forall`` () =
    assertFlagRewrite
        "let f (xs: int list) =\n    let mutable ok = true\n    for x in xs do\n        if x < 0 then ok <- false\n    ok"
        "let ok = xs |> List.forall (fun x -> not (x < 0))"

[<Fact>]
let ``a negated predicate in the forall dual loses its not`` () =
    assertFlagRewrite
        "let valid (x: int) = x >= 0\nlet f (xs: int list) =\n    let mutable ok = true\n    for x in xs do\n        if not (valid x) then ok <- false\n    ok"
        "let ok = xs |> List.forall (fun x -> (valid x))"

[<Fact>]
let ``a second statement in the loop body keeps the mutable`` () =
    Assert.Empty(
        flagLoopsIn
            "let f (xs: int list) =\n    let mutable found = false\n    let mutable count = 0\n    for x in xs do\n        count <- count + 1\n        if x > 3 then found <- true\n    found"
    )

[<Fact>]
let ``a side-effecting predicate keeps the mutable`` () =
    // exists short-circuits — a predicate with visible effects would run
    // fewer times after the rewrite
    Assert.Empty(
        flagLoopsIn
            "let f (xs: int list) =\n    let mutable found = false\n    let mutable seen = 0\n    for x in xs do\n        if (seen <- seen + 1; x > 3) then found <- true\n    found, seen"
    )

[<Fact>]
let ``an else branch keeps the mutable`` () =
    Assert.Empty(
        flagLoopsIn
            "let g () = ()\nlet f (xs: int list) =\n    let mutable found = false\n    for x in xs do\n        if x > 3 then found <- true else g ()\n    found"
    )

[<Fact>]
let ``flag reassignment after the loop keeps the mutable`` () =
    Assert.Empty(
        flagLoopsIn
            "let f (xs: int list) =\n    let mutable found = false\n    for x in xs do\n        if x > 3 then found <- true\n    found <- found && xs.Length > 1\n    found"
    )

[<Fact>]
let ``a predicate reading the flag keeps the mutable`` () =
    Assert.Empty(
        flagLoopsIn
            "let f (xs: int list) =\n    let mutable found = false\n    for x in xs do\n        if not found && x > 3 then found <- true\n    found"
    )

[<Fact>]
let ``a non-generic IEnumerable source keeps the mutable`` () =
    // `for` accepts it; List/Array/Seq.exists do not
    Assert.Empty(
        flagLoopsIn
            "let f (values: System.Collections.IEnumerable) =\n    let mutable found = false\n    for v in values do\n        if hash v > 3 then found <- true\n    found"
    )

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
let ``the inverted did-it-throw probe stays quiet too`` () =
    Assert.Empty(
        swallowedIn
            "module Test\nlet throws (act: unit -> unit) =\n    try\n        act ()\n        false\n    with _ -> true"
    )

[<Fact>]
let ``a false fallback on a non-probe body is a disguise`` () =
    // the body computes a real bool; the catch-all rewrites failure as
    // `false`, indistinguishable from an honest negative
    match
        swallowedIn
            "module Test\nlet isValid (parse: string -> bool) (s: string) =\n    try parse s\n    with _ -> false"
    with
    | [ s ] -> Assert.Equal(Some "false", s.FallbackText)
    | other -> failwithf "Expected one swallowed-exception finding, got %A" other

[<Fact>]
let ``a ValueNone fallback is a disguise`` () =
    match
        swallowedIn
            "module Test\nlet tryRead (read: unit -> int) =\n    try ValueSome(read ())\n    with _ -> ValueNone"
    with
    | [ s ] -> Assert.Equal(Some "ValueNone", s.FallbackText)
    | other -> failwithf "Expected one swallowed-exception finding, got %A" other

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
    | [ s ] -> Assert.Equal("let acc = xs |> List.map (fun x -> string x) |> String.concat \"\"", s.ReplacementText)
    | other -> failwithf "Expected exactly one mapped string concat, got %A" other

[<Fact>]
let ``a non-empty seed prefixes the concatenation`` () =
    let folds, _ =
        accumulationIn
            "module Test\nlet render (xs: string list) =\n    let mutable acc = \"head:\"\n    for x in xs do\n        acc <- acc + x\n    acc"

    match folds with
    | [ s ] -> Assert.Equal("let acc = \"head:\" + (xs |> String.concat \"\")", s.ReplacementText)
    | other -> failwithf "Expected exactly one seeded string concat, got %A" other

[<Fact>]
let ``ValueTask Result blocks like Task Result`` () =
    // post-core BCL async I/O returns ValueTask everywhere
    let suggestions =
        blockingIn "let f (vt: System.Threading.Tasks.ValueTask<int>) =\n    task {\n        return vt.Result\n    }"

    match suggestions with
    | [ s ] -> Assert.Equal(SyncOverAsync.BlockKind.TaskResult, s.Kind)
    | other -> failwithf "Expected exactly one ValueTask.Result note, got %A" other

[<Fact>]
let ``Task WaitAll is a blocking wait too`` () =
    let suggestions =
        blockingIn
            "let f (t1: System.Threading.Tasks.Task) =\n    task {\n        System.Threading.Tasks.Task.WaitAll [| t1 |]\n        return 1\n    }"

    match suggestions with
    | [ s ] -> Assert.Equal(SyncOverAsync.BlockKind.TaskWait, s.Kind)
    | other -> failwithf "Expected exactly one WaitAll note, got %A" other

[<Fact>]
let ``a Thread.Sleep inside a nested seq gets no do-fix`` () =
    // `do!` in the seq body would call a Bind the seq builder lacks
    let suggestions =
        blockingIn
            "let f () =\n    async {\n        let xs = seq {\n            System.Threading.Thread.Sleep 100\n            yield 1\n        }\n        return Seq.length xs\n    }"

    Assert.NotEmpty suggestions
    Assert.True(suggestions |> List.forall (fun s -> s.Fix.IsNone))

[<Fact>]
let ``a field-held concurrent queue count is an emptiness check too`` () =
    let suggestions =
        countsIn
            "type H() =\n    member val Queue = System.Collections.Concurrent.ConcurrentQueue<int>() with get\n    member this.Check() = this.Queue.Count = 0"

    match suggestions with
    | [ s ] -> Assert.Equal("this.Queue.IsEmpty", s.ReplacementText)
    | other -> failwithf "Expected exactly one count note, got %A" other

[<Fact>]
let ``a recursive member yielding itself through seq is noted`` () =
    // members are implicitly recursive — no `rec` keyword to find — and
    // OO-style tree APIs are where recursive seqs live
    let suggestions =
        recursiveSeqIn
            "module Test\ntype Node(children: Node list) =\n    member this.Descendants() : seq<int> = seq {\n        yield 1\n        for c in children do\n            yield! c.Descendants()\n    }"

    match suggestions with
    | [ s ] -> Assert.Equal("Descendants", s.FunctionName)
    | other -> failwithf "Expected exactly one recursive-member note, got %A" other

[<Fact>]
let ``quadratic List.append in a loop is noted`` () =
    let _, quadratics =
        accumulationIn
            "let f (xs: int list) =\n    let mutable acc: int list = []\n    for x in xs do\n        if x > 0 then acc <- List.append acc [ x ]\n    acc"

    match quadratics with
    | [ s ] -> Assert.Equal("acc", s.Name)
    | other -> failwithf "Expected exactly one List.append note, got %A" other

[<Fact>]
let ``quadratic append through a ref cell is noted`` () =
    let _, quadratics =
        accumulationIn
            "let f (xs: int list) =\n    let acc = ref ([]: int list)\n    for x in xs do\n        acc.Value <- acc.Value @ [ x ]\n    acc.Value"

    match quadratics with
    | [ s ] -> Assert.Equal("acc", s.Name)
    | other -> failwithf "Expected exactly one ref-cell note, got %A" other

[<Fact>]
let ``a plain seq source still sums with the Seq module`` () =
    // the module-resolved output names List/Array when it can; a true
    // seq has nothing better than Seq.sum
    assertFold
        "let f (xs: int seq) =\n    let mutable total = 0\n    for x in xs do\n        total <- total + x\n    total"
        "let total = xs |> Seq.sum"

[<Fact>]
let ``quadratic string building in a while loop is noted`` () =
    // measured: 57.8µs and 1MB per 1000 pieces against 1.6µs/4.6KB for a
    // StringBuilder — the worst string builder there is
    let _, quadratics =
        accumulationIn
            "let f (next: unit -> string option) =\n    let mutable acc = \"\"\n    let mutable go = true\n    while go do\n        match next () with\n        | Some s -> acc <- acc + s\n        | None -> go <- false\n    acc"

    match quadratics with
    | [ s ] ->
        Assert.Equal("acc", s.Name)
        Assert.Equal(Accumulation.QuadraticKind.Str, s.Kind)
    | other -> failwithf "Expected exactly one string-quadratic note, got %A" other

[<Fact>]
let ``numeric accumulation with plus is ordinary code`` () =
    let _, quadratics =
        accumulationIn
            "let f (next: unit -> int option) =\n    let mutable total = 0\n    let mutable go = true\n    while go do\n        match next () with\n        | Some n -> total <- total + n\n        | None -> go <- false\n    total"

    Assert.Empty quadratics

[<Fact>]
let ``the FR0050 string shape gets the fix, not the note`` () =
    // the fold fix rewrites this whole shape into one String.concat; a
    // note on the same site would nag about code the fix removes
    let folds, quadratics =
        accumulationIn
            "let render (xs: int list) =\n    let mutable acc = \"\"\n    for x in xs do\n        acc <- acc + string x\n    acc"

    Assert.Single folds |> ignore
    Assert.Empty quadratics

[<Fact>]
let ``a seq source materializes before String concat`` () =
    // the lazy-seq path through String.concat measured 42.7µs/194KB per
    // 1000 pieces against 2.6µs/2KB once materialized
    assertFold
        "let render (xs: int seq) =\n    let mutable acc = \"\"\n    for x in xs do\n        acc <- acc + string x\n    acc"
        "let acc = xs |> Seq.map (fun x -> string x) |> Seq.toArray |> String.concat \"\""

[<Fact>]
let ``a pure let prefix folds into the exists lambda`` () =
    // the opensSystem shape from our own code review: a let-bound
    // projection before the flag test is still an exists question
    assertFlagRewrite
        "let f (lines: string list) =\n    let mutable found = false\n    for l in lines do\n        let t = l.Trim()\n        if t = \"open System\" then found <- true\n    found"
        "let found = lines |> List.exists (fun l -> let t = l.Trim() in t = \"open System\")"

[<Fact>]
let ``a mutable let in the body keeps the flag loop`` () =
    Assert.Empty(
        flagLoopsIn
            "let f (xs: int list) =\n    let mutable found = false\n    for x in xs do\n        let mutable y = x + 1\n        if y > 3 then found <- true\n    found"
    )
