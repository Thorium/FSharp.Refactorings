module FSharp.Refactor.Tests.CorrectnessRulesTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

// ---- FR0044 Reraise ----

let private reraiseIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    Reraise.find tree sourceText checkResults

let private assertReraise (source: string) =
    match reraiseIn source with
    | [ s ] ->
        let patched = applyEdit source s.Range "reraise ()"
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one reraise suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``raise of the caught exception becomes reraise`` () =
    assertReraise
        "let f (act: unit -> int) =\n    try act ()\n    with ex ->\n        printfn \"%s\" ex.Message\n        raise ex"

[<Fact>]
let ``typed catch with as-binding is also covered`` () =
    assertReraise
        "let f (act: unit -> int) =\n    try act ()\n    with :? System.IO.IOException as ex ->\n        printfn \"%s\" ex.Message\n        raise ex"

[<Fact>]
let ``raising a different exception is intentional`` () =
    Assert.Empty(
        reraiseIn
            "let f (act: unit -> int) =\n    try act ()\n    with ex ->\n        raise (System.InvalidOperationException(\"wrap\", ex))"
    )

[<Fact>]
let ``raise inside a lambda cannot become reraise`` () =
    // reraise () does not compile inside a closure
    Assert.Empty(
        reraiseIn
            "let f (act: unit -> int) (defer: (unit -> int) -> int) =\n    try act ()\n    with ex -> defer (fun () -> raise ex)"
    )

[<Fact>]
let ``raise in a handler inside a computation expression stays put`` () =
    // from the corpus (SQLProvider Providers.SQLite): a try-with inside
    // task { } desugars its handler into a lambda passed to builder.TryWith,
    // where reraise () is error FS0413
    Assert.Empty(
        reraiseIn
            "let f (act: unit -> System.Threading.Tasks.Task) =\n    task {\n        try do! act ()\n        with ex ->\n            printfn \"%s\" ex.Message\n            raise ex\n    }"
    )

[<Fact>]
let ``a lambda body inside a computation expression is ordinary code again`` () =
    // the lambda compiles to its own method; its handler is a real catch
    // block again, so reraise () is fine there
    assertReraise
        "let f (act: unit -> int) =\n    async {\n        let g = fun () -> try act () with ex -> raise ex\n        return g ()\n    }"

[<Fact>]
let ``raise inside a nested handler refers to the inner exception`` () =
    Assert.Empty(
        reraiseIn
            "let f (act: unit -> int) (cleanup: unit -> int) =\n    try act ()\n    with ex ->\n        try cleanup ()\n        with _ -> raise ex"
    )

// ---- FR0045 NaNComparison ----

let private nanIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    NaNComparison.find tree sourceText checkResults

let private assertNaN (source: string) (expectedReplacement: string) =
    match nanIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one NaN suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``equality with nan becomes IsNaN`` () =
    assertNaN "let f (x: float) = x = nan" "System.Double.IsNaN x"

[<Fact>]
let ``inequality with Double NaN becomes negated IsNaN`` () =
    assertNaN "let f (x: float) = x <> System.Double.NaN" "not (System.Double.IsNaN x)"

[<Fact>]
let ``reversed operand order is covered`` () =
    assertNaN "let f (x: float) = nan = x" "System.Double.IsNaN x"

[<Fact>]
let ``single NaN uses Single IsNaN`` () =
    assertNaN "let f (x: float32) = x = System.Single.NaN" "System.Single.IsNaN x"

[<Fact>]
let ``ordinary float equality is left alone`` () =
    Assert.Empty(nanIn "let f (x: float) (y: float) = x = y")

// ---- FR0046 WeakLock ----

let private locksIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    WeakLock.find tree sourceText checkResults

[<Fact>]
let ``locking on a string literal is noted`` () =
    match locksIn "let f () = lock \"cache\" (fun () -> 1)" with
    | [ s ] -> Assert.Equal(WeakLock.WeakKind.StringValue, s.Kind)
    | other -> failwithf "Expected exactly one weak-lock note, got %A" other

[<Fact>]
let ``locking on typeof is noted`` () =
    match locksIn "let f () = lock typeof<string> (fun () -> 1)" with
    | [ s ] -> Assert.Equal(WeakLock.WeakKind.TypeObject, s.Kind)
    | other -> failwithf "Expected exactly one typeof-lock note, got %A" other

[<Fact>]
let ``locking on a dedicated object is fine`` () =
    Assert.Empty(locksIn "let lockObj = obj ()\nlet f () = lock lockObj (fun () -> 1)")

// ---- FR0047 UndisposedField ----

let private designIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    ObjectDesign.find tree sourceText checkResults

[<Fact>]
let ``disposable field missing from Dispose is noted`` () =
    let _, _, undisposed =
        designIn
            "type Holder() =\n    let stream = new System.IO.MemoryStream()\n    let backup = new System.IO.MemoryStream()\n    member _.Size = stream.Length + backup.Length\n\n    interface System.IDisposable with\n        member _.Dispose() = stream.Dispose()"

    match undisposed with
    | [ s ] -> Assert.Equal("backup", s.FieldName)
    | other -> failwithf "Expected exactly one undisposed-field note, got %A" other

[<Fact>]
let ``fields the Dispose touches are fine`` () =
    let _, _, undisposed =
        designIn
            "type Holder() =\n    let stream = new System.IO.MemoryStream()\n    member _.Size = stream.Length\n\n    interface System.IDisposable with\n        member _.Dispose() = stream.Dispose()"

    Assert.Empty undisposed

// ---- FR0048 FormatArgs ----

let private formatsIn (source: string) =
    let tree, sourceText = parse source
    FormatArgs.find tree sourceText

[<Fact>]
let ``missing format argument is noted`` () =
    match formatsIn "module Test\nlet f (x: int) = System.String.Format(\"{0} of {1}\", x)" with
    | [ s ] ->
        Assert.Equal(1, s.MissingIndex)
        Assert.Equal(1, s.ArgCount)
    | other -> failwithf "Expected exactly one format note, got %A" other

[<Fact>]
let ``matching arguments are fine`` () =
    Assert.Empty(formatsIn "module Test\nlet f (x: int) (y: int) = System.String.Format(\"{0} of {1}\", x, y)")

[<Fact>]
let ``escaped braces are not placeholders`` () =
    Assert.Empty(formatsIn "module Test\nlet f (x: int) = System.String.Format(\"{{0}} literal {0}\", x)")

// ---- FR0105 CheckedArithmetic ----

let private checkedIn (source: string) =
    let tree, sourceText = parse source
    CheckedArithmetic.find tree sourceText

[<Fact>]
let ``a near-limit constant in an addition is noted`` () =
    match checkedIn "module Test\nlet f (balance: int) = balance + 2_000_000_000" with
    | [ s ] -> Assert.Equal("2_000_000_000", s.ConstantText)
    | other -> failwithf "Expected exactly one overflow note, got %A" other

[<Fact>]
let ``ordinary constants are ordinary`` () =
    Assert.Empty(checkedIn "module Test\nlet f (n: int) = n + 1000")

[<Fact>]
let ``a hex constant is a mask, not a magnitude`` () =
    Assert.Empty(checkedIn "module Test\nlet f (n: int) = n + 0x7FFFFFFF")

[<Fact>]
let ``a file that opens Checked has made its choice`` () =
    Assert.Empty(
        checkedIn
            "module Test\nopen Microsoft.FSharp.Core.Operators.Checked\nlet f (balance: int) = balance + 2_000_000_000"
    )

[<Fact>]
let ``a near-limit int64 multiplication is noted`` () =
    match checkedIn "module Test\nlet f (n: int64) = n * 600_000_000_000_000_000L" with
    | [ s ] -> Assert.Equal("600_000_000_000_000_000L", s.ConstantText)
    | other -> failwithf "Expected exactly one int64 note, got %A" other

[<Fact>]
let ``FR0032: a field the type disposes itself is managed, not ownerless`` () =
    // FSharp.Data's FileWatcher: the watcher is disposed when the last
    // subscriber leaves — a protocol, not a leak
    let disposables, _, _ =
        designIn
            "module Test\nopen System.IO\ntype Watcher(path: string) =\n    let watcher = new FileSystemWatcher(path)\n    let mutable count = 1\n    member _.Unsubscribe() =\n        count <- count - 1\n        if count = 0 then watcher.Dispose()"

    Assert.Empty disposables

[<Fact>]
let ``FR0047: a Dispose that delegates to DisposeAsync disposes through the async body`` () =
    // FsAutoComplete's ServerProgressReport: cts is disposed in DisposeAsync,
    // and Dispose only forwards — the field is not missed
    let _, _, undisposed =
        designIn
            "module Test\nopen System\nopen System.Threading\nopen System.Threading.Tasks\ntype Reporter() =\n    let cts = new CancellationTokenSource()\n    interface IAsyncDisposable with\n        member _.DisposeAsync() =\n            cts.Dispose()\n            ValueTask()\n    interface IDisposable with\n        member x.Dispose() = (x :> IAsyncDisposable).DisposeAsync() |> ignore"

    Assert.Empty undisposed

[<Fact>]
let ``FR0105: MaxValue plus something overflows, MaxValue minus something is a sentinel`` () =
    match
        checkedIn "module Test\nlet a (n: int) = System.Int32.MaxValue + n\nlet b (n: int) = System.Int32.MaxValue - n"
    with
    | [ s ] -> Assert.Equal("System.Int32.MaxValue", s.ConstantText)
    | other -> failwithf "Expected one near-limit finding, got %A" other

[<Fact>]
let ``FR0046: locking this is a weak lock, spelled with a pipe too`` () =
    match
        locksIn
            "module Test\ntype Cache() =\n    let mutable n = 0\n    member this.Bump() =\n        lock this\n        <| fun () -> n <- n + 1"
    with
    | [ s ] ->
        Assert.Equal(WeakLock.WeakKind.SelfObject, s.Kind)
        Assert.Empty s.Fix // a member has no module-level slot for a lock object
    | other -> failwithf "Expected one weak-lock finding, got %A" other

[<Fact>]
let ``FR0046: a lock on stdout is noted without a fix`` () =
    match locksIn "module Test\nlet say (s: string) = lock stdout (fun () -> printfn \"%s\" s)" with
    | [ s ] ->
        Assert.Equal(WeakLock.WeakKind.SharedSingleton "stdout", s.Kind)
        Assert.Empty s.Fix
    | other -> failwithf "Expected one shared-singleton finding, got %A" other

[<Fact>]
let ``FR0046: a lock on a string literal gets a lock object before the binding`` () =
    let source =
        "module Test\nlet mutable count = 0\nlet bump () = lock \"cache\" (fun () -> count <- count + 1)"

    match locksIn source with
    | [ s ] ->
        Assert.Equal(WeakLock.WeakKind.StringValue, s.Kind)

        let patched =
            s.Fix
            |> List.sortByDescending (fun (r, _, _) -> r.StartLine, r.StartColumn)
            |> List.fold (fun acc (r, _, replacement) -> applyEdit acc r replacement) source

        Assert.Equal(
            "module Test\nlet mutable count = 0\nlet private bumpLock = obj ()\n\nlet bump () = lock bumpLock (fun () -> count <- count + 1)",
            patched
        )

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one weak-lock finding, got %A" other

[<Fact>]
let ``FR0046: a lock on a module string value gets its lock object next to that value`` () =
    let source =
        "module Test\nlet key = \"cache\"\nlet mutable count = 0\nlet bump () = lock key (fun () -> count <- count + 1)"

    match locksIn source with
    | [ s ] ->
        let patched =
            s.Fix
            |> List.sortByDescending (fun (r, _, _) -> r.StartLine, r.StartColumn)
            |> List.fold (fun acc (r, _, replacement) -> applyEdit acc r replacement) source

        Assert.Equal(
            "module Test\nlet key = \"cache\"\nlet private keyLock = obj ()\nlet mutable count = 0\nlet bump () = lock keyLock (fun () -> count <- count + 1)",
            patched
        )

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one weak-lock finding, got %A" other

[<Fact>]
let ``FR0105: a million-scale multiplier is a unit conversion that overflows int32`` () =
    match checkedIn "module Test\nlet micros (seconds: int) = seconds * 1_000_000" with
    | [ s ] ->
        Assert.Equal(CheckedArithmetic.OverflowKind.ScaleFactor, s.Kind)
        Assert.Equal("1_000_000", s.ConstantText)
    | other -> failwithf "Expected one scale-factor finding, got %A" other

[<Fact>]
let ``FR0105: the editor's first offer widens to int64 and typechecks, the second goes through Checked`` () =
    let source = "module Test\nlet micros (seconds: int) = seconds * 1_000_000"

    match checkedIn source with
    | [ s ] ->
        match s.WidenFix, s.CheckedFix with
        | Some(r, _, widened), Some(r2, _, checked') ->
            Assert.Equal("int64 seconds * 1_000_000L |> Checked.int", widened)
            let patched = applyEdit source r widened
            Assert.True(typechecksCleanly patched, $"Widened source does not typecheck:\n%s{patched}")
            Assert.Equal("Checked.( * ) seconds 1_000_000", checked')
            let patched2 = applyEdit source r2 checked'
            Assert.True(typechecksCleanly patched2, $"Checked source does not typecheck:\n%s{patched2}")
        | other -> failwithf "Expected both offers, got %A" other
    | other -> failwithf "Expected one finding, got %A" other

[<Fact>]
let ``FR0105: a MaxValue expression is noted without a widening offer`` () =
    match checkedIn "module Test\nlet next (n: int) = System.Int32.MaxValue + n" with
    | [ s ] ->
        Assert.Equal(CheckedArithmetic.OverflowKind.LimitConstant, s.Kind)
        Assert.Equal(None, s.WidenFix)
    | other -> failwithf "Expected one limit-constant finding, got %A" other

[<Fact>]
let ``FR0105: the widening rewrites the whole arithmetic expression and narrows back through Checked`` () =
    let source = "module Test\nlet x = (1000000 * 1000000 + 5) / 100000"

    match checkedIn source with
    | [ s ] ->
        match s.WidenFix with
        | Some(r, _, widened) ->
            Assert.Equal("(1000000L * 1000000L + 5L) / 100000L |> Checked.int", widened)
            let patched = applyEdit source r widened
            Assert.True(typechecksCleanly patched, $"Widened source does not typecheck:\n%s{patched}")
        | None -> failwith "Expected the widening offer"
    | other -> failwithf "Expected one finding, got %A" other

[<Fact>]
let ``FR0046: a lock in a nested module gets its lock object in that module, indented`` () =
    let source =
        "module Test\nmodule Inner =\n    let mutable count = 0\n    let bump () = lock \"cache\" (fun () -> count <- count + 1)"

    match locksIn source with
    | [ s ] ->
        let patched =
            s.Fix
            |> List.sortByDescending (fun (r, _, _) -> r.StartLine, r.StartColumn)
            |> List.fold (fun acc (r, _, replacement) -> applyEdit acc r replacement) source

        Assert.Equal(
            "module Test\nmodule Inner =\n    let mutable count = 0\n    let private bumpLock = obj ()\n\n    let bump () = lock bumpLock (fun () -> count <- count + 1)",
            patched
        )

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one weak-lock finding, got %A" other
