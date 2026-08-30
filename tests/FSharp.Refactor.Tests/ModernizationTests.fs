module FSharp.Refactor.Tests.ModernizationTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private applyAll (source: string) (edits: (FSharp.Compiler.Text.range * string * string) list) =
    edits
    |> List.sortByDescending (fun (r, _, _) -> r.StartLine, r.StartColumn)
    |> List.fold (fun acc (r, _, replacement) -> applyEdit acc r replacement) source

// ---- FR0073 MatchBang ----

let private matchBangsIn (source: string) =
    let tree, sourceText = parse source
    MatchBangRule.find tree sourceText

let private assertMatchBang (source: string) (expectedPatched: string) =
    match matchBangsIn source with
    | [ s ] ->
        let patched = applyAll source s.Edits
        Assert.Equal(expectedPatched, patched)
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one match! note, got %A" other

[<Fact>]
let ``let!-then-match collapses to match!`` () =
    assertMatchBang
        "module Test\nlet fetch () = async { return Some 1 }\nlet run () =\n    async {\n        let! x = fetch ()\n        match x with\n        | Some v -> return v\n        | None -> return 0\n    }"
        "module Test\nlet fetch () = async { return Some 1 }\nlet run () =\n    async {\n        match! fetch () with\n        | Some v -> return v\n        | None -> return 0\n    }"

[<Fact>]
let ``a binder used in a clause body must stay`` () =
    Assert.Empty(
        matchBangsIn
            "module Test\nlet fetch () = async { return Some 1 }\nlet run () =\n    async {\n        let! x = fetch ()\n        match x with\n        | Some _ -> return x\n        | None -> return None\n    }"
    )

[<Fact>]
let ``a use! binding manages a resource and stays`` () =
    Assert.Empty(
        matchBangsIn
            "module Test\nopen System\nlet acquire () = async { return { new IDisposable with member _.Dispose() = () } }\nlet run () =\n    async {\n        use! d = acquire ()\n        match d with\n        | _ -> return 1\n    }"
    )

// ---- FR0078 WhileBang ----

let private whileBangsIn (source: string) =
    let tree, sourceText = parse source
    MatchBangRule.findWhileBang tree sourceText

[<Fact>]
let ``the three-part mutable-condition loop collapses to while!`` () =
    match
        whileBangsIn
            "module Test\nlet check () = async { return false }\nlet step () = async { return () }\nlet run () =\n    async {\n        let! first = check ()\n        let mutable go = first\n        while go do\n            do! step ()\n            let! next = check ()\n            go <- next\n    }"
    with
    | [ s ] ->
        let patched =
            applyAll
                "module Test\nlet check () = async { return false }\nlet step () = async { return () }\nlet run () =\n    async {\n        let! first = check ()\n        let mutable go = first\n        while go do\n            do! step ()\n            let! next = check ()\n            go <- next\n    }"
                s.Edits

        Assert.Equal(
            "module Test\nlet check () = async { return false }\nlet step () = async { return () }\nlet run () =\n    async {\n        while! check () do\n            do! step ()\n    }",
            patched
        )

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one while! note, got %A" other

[<Fact>]
let ``a stale-bool while without the rebind is not while!`` () =
    // while! re-evaluates each iteration; this shape does not
    Assert.Empty(
        whileBangsIn
            "module Test\nlet check () = async { return false }\nlet run () =\n    async {\n        let! first = check ()\n        let mutable go = first\n        while go do\n            printfn \"tick\"\n    }"
    )

[<Fact>]
let ``different condition computations stay apart`` () =
    Assert.Empty(
        whileBangsIn
            "module Test\nlet check () = async { return false }\nlet other () = async { return false }\nlet run () =\n    async {\n        let! first = check ()\n        let mutable go = first\n        while go do\n            printfn \"tick\"\n            let! next = other ()\n            go <- next\n    }"
    )

// ---- FR0074 NestedRecordUpdate ----

let private nestedIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    NestedRecordUpdate.find tree sourceText checkResults

let private assertFlattened (source: string) (expectedReplacement: string) =
    match nestedIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one flatten note, got %A" other

[<Fact>]
let ``a nested copy-and-update flattens to a path`` () =
    assertFlattened
        "module Test\ntype Inner = { Y: int; Z: int }\ntype Outer = { X: Inner; N: int }\nlet f (r: Outer) (v: int) = { r with X = { r.X with Y = v } }"
        "X.Y = v"

[<Fact>]
let ``multiple inner fields flatten side by side`` () =
    assertFlattened
        "module Test\ntype Inner = { Y: int; Z: int }\ntype Outer = { X: Inner; N: int }\nlet f (r: Outer) (v: int) = { r with X = { r.X with Y = v; Z = v + 1 } }"
        "X.Y = v; X.Z = v + 1"

[<Fact>]
let ``two levels flatten to a deep path`` () =
    assertFlattened
        "module Test\ntype L3 = { V: int }\ntype L2 = { Inner: L3 }\ntype L1 = { Mid: L2 }\nlet f (r: L1) (v: int) = { r with Mid = { r.Mid with Inner = { r.Mid.Inner with V = v } } }"
        "Mid.Inner.V = v"

[<Fact>]
let ``a field named after a type keeps the nested form`` () =
    // `{ r with B.A.V = v }` would resolve B as the TYPE and fail to
    // compile — the field-named-after-its-type pattern stays nested
    Assert.Empty(
        nestedIn
            "module Test\ntype A = { V: int }\ntype B = { A: A }\ntype C = { B: B }\nlet f (r: C) (v: int) = { r with B = { r.B with A = { r.B.A with V = v } } }"
    )

[<Fact>]
let ``a cross-record inner copy stays`` () =
    // the inner source is a DIFFERENT record, not r.X — nothing to flatten
    Assert.Empty(
        nestedIn
            "module Test\ntype Inner = { Y: int; Z: int }\ntype Outer = { X: Inner; N: int }\nlet f (r: Outer) (q: Inner) (v: int) = { r with X = { q with Y = v } }"
    )

// ---- FR0075 UseBinding ----

let private useBindingsIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    UseBinding.find tree sourceText checkResults

[<Fact>]
let ``a contained local disposable becomes a use binding`` () =
    match
        useBindingsIn
            "module Test\nopen System.IO\nlet read (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    let b = stream.ReadByte()\n    b + 1"
    with
    | [ s ] ->
        Assert.Equal(Some("let", "use"), s.Fix)

        let patched =
            applyEdit
                "module Test\nopen System.IO\nlet read (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    let b = stream.ReadByte()\n    b + 1"
                s.Range
                "use"

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one use-binding fix, got %A" other

[<Fact>]
let ``a disposable passed on bare gets advice only`` () =
    match
        useBindingsIn
            "module Test\nopen System.IO\nlet handOff (sink: FileStream -> unit) (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    sink stream"
    with
    | [ s ] -> Assert.Equal(None, s.Fix)
    | other -> failwithf "Expected exactly one advisory, got %A" other

[<Fact>]
let ``a disposable returned to the caller must stay a let`` () =
    // `use` here would dispose the stream before the caller ever saw it.
    // The caller is the one that should write `use`, so this side stays a
    // `let` and only gets the advisory
    match
        useBindingsIn
            "module Test\nopen System.IO\nlet openStream (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    stream"
    with
    | [ s ] -> Assert.Equal(None, s.Fix)
    | other -> failwithf "Expected exactly one return-escape advisory, got %A" other

[<Fact>]
let ``a disposable handed to a returned wrapper stays a let`` () =
    // StreamReader takes ownership and outlives this scope
    match
        useBindingsIn
            "module Test\nopen System.IO\nlet openReader (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    new StreamReader(stream)"
    with
    | [ s ] -> Assert.Equal(None, s.Fix)
    | other -> failwithf "Expected exactly one wrapper-escape advisory, got %A" other

[<Fact>]
let ``a manually disposed local is managed already`` () =
    Assert.Empty(
        useBindingsIn
            "module Test\nopen System.IO\nlet read (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    let b = stream.ReadByte()\n    stream.Dispose()\n    b"
    )

[<Fact>]
let ``a non-disposable local is fine`` () =
    Assert.Empty(
        useBindingsIn
            "module Test\nlet f () =\n    let sb = new System.Text.StringBuilder()\n    sb.Append('x') |> ignore\n    sb.Length"
    )

// ---- FR0076 MapIgnore ----

let private mapIgnoresIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    MapIgnore.find tree sourceText checkResults

[<Fact>]
let ``List map piped to ignore becomes iter`` () =
    match mapIgnoresIn "module Test\nlet f (g: int -> int) (xs: int list) = xs |> List.map g |> ignore" with
    | [ s ] ->
        Assert.Equal(Some "xs |> List.iter (g >> ignore)", s.ReplacementText)

        let patched =
            applyEdit
                "module Test\nlet f (g: int -> int) (xs: int list) = xs |> List.map g |> ignore"
                s.Range
                s.ReplacementText.Value

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one map-ignore fix, got %A" other

[<Fact>]
let ``Seq map piped to ignore is the lazy bug and gets advice`` () =
    match mapIgnoresIn "module Test\nlet f (g: int -> int) (xs: seq<int>) = xs |> Seq.map g |> ignore" with
    | [ s ] ->
        Assert.Equal("Seq", s.ModuleName)
        Assert.Equal(None, s.ReplacementText)
    | other -> failwithf "Expected exactly one lazy advisory, got %A" other

[<Fact>]
let ``a used map result is fine`` () =
    Assert.Empty(mapIgnoresIn "module Test\nlet f (g: int -> int) (xs: int list) = xs |> List.map g |> List.sum")

[<Fact>]
let ``a shadowed map is left alone`` () =
    Assert.Empty(
        mapIgnoresIn
            "module Test\nmodule List =\n    let map (f: int -> int) (xs: int list) = xs\nlet f (g: int -> int) (xs: int list) = xs |> List.map g |> ignore"
    )

[<Fact>]
let ``a condition computation reading the mutable binder stays`` () =
    // `let! next = step go` — deleting `go` would strand the computation
    Assert.Empty(
        whileBangsIn
            "module Test\nlet step (b: bool) = async { return not b }\nlet run () =\n    async {\n        let! first = step true\n        let mutable go = first\n        while go do\n            printfn \"tick\"\n            let! next = step go\n            go <- next\n    }"
    )

// ---- FR0079 SingleAwaitable ----

let private singlesIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    SingleAwaitable.find tree sourceText checkResults

[<Fact>]
let ``WhenAll over a single-task literal is noted`` () =
    match singlesIn "module Test\nopen System.Threading.Tasks\nlet f (t: Task<int>) = Task.WhenAll [| t |]" with
    | [ s ] -> Assert.Equal("Task.WhenAll", s.CallName)
    | other -> failwithf "Expected exactly one WhenAll note, got %A" other

[<Fact>]
let ``Parallel over a single computation is noted`` () =
    match singlesIn "module Test\nlet f (c: Async<int>) = Async.Parallel [ c ]" with
    | [ s ] -> Assert.Equal("Async.Parallel", s.CallName)
    | other -> failwithf "Expected exactly one Parallel note, got %A" other

[<Fact>]
let ``two tasks genuinely combine`` () =
    Assert.Empty(
        singlesIn
            "module Test\nopen System.Threading.Tasks\nlet f (a: Task<int>) (b: Task<int>) = Task.WhenAll [| a; b |]"
    )

[<Fact>]
let ``a comprehension may yield any number`` () =
    Assert.Empty(singlesIn "module Test\nlet f (cs: Async<int> list) = Async.Parallel [ for c in cs -> c ]")

// ---- FR0077 ImplementMissing ----

let private missingIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    ImplementMissing.find tree sourceText checkResults

[<Fact>]
let ``missing interface members get NotImplementedException stubs`` () =
    let source =
        "module Test\ntype IThing =\n    abstract member Go: unit -> int\n    abstract member Stop: string -> unit\n    abstract member Name: string\n\nlet t =\n    { new IThing with\n        member _.Go() = 1 }"

    match missingIn source with
    | [ s ] ->
        Assert.Equal<string list>([ "Stop"; "Name" ] |> List.sort, s.MissingNames |> List.sort)
        let patched = applyEdit source s.Range s.InsertText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one implement-missing fix, got %A" other

[<Fact>]
let ``an inherited interface stubs in its own section`` () =
    let source =
        "module Test\nopen System\ntype IRes =\n    inherit IDisposable\n    abstract member Load: unit -> int\n\nlet r =\n    { new IRes with\n        member _.Load() = 1 }"

    match missingIn source with
    | [ s ] ->
        Assert.Contains("Dispose", s.MissingNames)
        Assert.Contains("interface IDisposable with", s.InsertText)
        let patched = applyEdit source s.Range s.InsertText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one inherited-stub fix, got %A" other

[<Fact>]
let ``members implemented in the main block satisfy inherited interfaces`` () =
    // from the corpus (SQLProvider Stubs): the IDbConnection stub implements
    // Dispose in the main block, which satisfies IDisposable — an extra
    // `interface IDisposable with` stub would double-implement it (FS0767).
    // The unrelated error keeps the file in FR0077's runs-on-broken-code path.
    Assert.Empty(
        missingIn
            "module Test\nopen System\ntype IRes2 =\n    inherit IDisposable\n    abstract member Load: unit -> int\n\nlet broken: int = \"s\"\n\nlet r =\n    { new IRes2 with\n        member _.Load() = 1\n        member _.Dispose() = () }"
    )

[<Fact>]
let ``a file that already type-checks is left alone`` () =
    // a clean file has nothing missing, whatever the name-matching
    // heuristics conclude — FR0077 exists to fix FS0366, not working code
    Assert.Empty(
        missingIn
            "module Test\nopen System\ntype IRes3 =\n    inherit IDisposable\n    abstract member Load: unit -> int\n\nlet r =\n    { new IRes3 with\n        member _.Load() = 1\n        member _.Dispose() = () }"
    )

[<Fact>]
let ``a complete object expression is quiet`` () =
    Assert.Empty(
        missingIn
            "module Test\ntype IThing2 =\n    abstract member Go: unit -> int\n\nlet t =\n    { new IThing2 with\n        member _.Go() = 1 }"
    )

[<Fact>]
let ``a property with getter and setter stubs both`` () =
    let source =
        "module Test\ntype IHolder =\n    abstract member Value: int with get, set\n    abstract member Touch: unit -> unit\n\nlet h =\n    { new IHolder with\n        member _.Touch() = () }"

    match missingIn source with
    | [ s ] ->
        Assert.Contains("with get () =", s.InsertText)
        Assert.Contains("and set _v =", s.InsertText)
        let patched = applyEdit source s.Range s.InsertText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one get-set stub fix, got %A" other

// ---- FR0080 TabIndentation ----

let private tabsIn (source: string) =
    let sourceText = FSharp.Compiler.Text.SourceText.ofString source
    TabIndentation.find "Test.fs" sourceText

[<Fact>]
let ``leading tabs expand to spaces line by line`` () =
    let source = "module Test\nlet f x =\n\tlet y = x + 1\n\ty + 1"

    match tabsIn source with
    | [ s ] ->
        Assert.Equal(2, s.Edits.Length)

        let patched =
            s.Edits
            |> List.sortByDescending (fun (r, _, _) -> r.StartLine)
            |> List.fold (fun acc (r, _, replacement) -> applyEdit acc r replacement) source

        Assert.Equal("module Test\nlet f x =\n    let y = x + 1\n    y + 1", patched)
        Assert.True(parsesCleanly patched, $"Patched source does not parse:\n%s{patched}")
    | other -> failwithf "Expected exactly one tab note, got %A" other

[<Fact>]
let ``a tab after code is not indentation`` () =
    Assert.Empty(tabsIn "module Test\nlet s = \"a\tb\"")

[<Fact>]
let ``multiline string literals make tabs ambiguous`` () =
    // the leading tab could be CONTENT of the triple-quoted literal
    Assert.Empty(tabsIn "module Test\nlet s = \"\"\"line\n\tstill the string\"\"\"\nlet f x =\n\tx + 1")

// ---- FR0081 PathSeparator ----

let private pathsIn (source: string) =
    let tree, sourceText = parse source
    PathSeparator.find tree sourceText

[<Fact>]
let ``a backslash-joined path is noted`` () =
    match pathsIn "module Test\nlet f (dir: string) (file: string) = dir + \"\\\\\" + file" with
    | [ s ] -> Assert.Equal("\\", s.Separator)
    | other -> failwithf "Expected exactly one path note, got %A" other

[<Fact>]
let ``a slash-joined path is noted`` () =
    match pathsIn "module Test\nlet f (root: string) (name: string) = root + \"/\" + name + \".txt\"" with
    | [ s ] -> Assert.Equal("/", s.Separator)
    | other -> failwithf "Expected exactly one slash note, got %A" other

[<Fact>]
let ``a url join is not a file path`` () =
    Assert.Empty(pathsIn "module Test\nlet f (baseUrl: string) (route: string) = baseUrl + \"/\" + route")

[<Fact>]
let ``a scheme literal is not a file path`` () =
    Assert.Empty(pathsIn "module Test\nlet f (host: string) = \"https://\" + host + \"/api\"")

[<Fact>]
let ``plain text concatenation is not a path`` () =
    Assert.Empty(pathsIn "module Test\nlet f (a: string) (b: string) = a + \", \" + b")

// ---- FR0082-FR0086 RedundantSyntax ----

let private syntaxIn (source: string) =
    let tree, sourceText = parse source
    RedundantSyntax.find tree sourceText

let private assertSyntaxFix (kind: RedundantSyntax.Kind) (source: string) (expectedPatched: string) =
    match syntaxIn source |> List.filter (fun s -> s.Kind = kind) with
    | [ s ] ->
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.Equal(expectedPatched, patched)
        Assert.True(parsesCleanly patched, $"Patched source does not parse:\n%s{patched}")
    | other -> failwithf "Expected exactly one %A fix, got %A" kind other

[<Fact>]
let ``the Attribute suffix is trimmed`` () =
    assertSyntaxFix
        RedundantSyntax.Kind.AttributeSuffix
        "module Test\n[<System.SerializableAttribute>]\ntype T = { X: int }"
        "module Test\n[<System.Serializable>]\ntype T = { X: int }"

[<Fact>]
let ``an attribute named exactly Attribute keeps its name`` () =
    Assert.Empty(
        syntaxIn "module Test\n[<System.Serializable>]\ntype T = { X: int }"
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.AttributeSuffix)
    )

[<Fact>]
let ``empty attribute parens go away`` () =
    assertSyntaxFix
        RedundantSyntax.Kind.AttributeParens
        "module Test\n[<System.Serializable()>]\ntype T = { X: int }"
        "module Test\n[<System.Serializable>]\ntype T = { X: int }"

[<Fact>]
let ``redundant backticks strip at use and binder sites`` () =
    match
        syntaxIn "module Test\nlet ``plain`` = 1\nlet f () = ``plain`` + 1"
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.Backticks)
    with
    | [ _; _ ] -> ()
    | other -> failwithf "Expected two backtick fixes, got %A" other

[<Fact>]
let ``necessary backticks stay`` () =
    Assert.Empty(
        syntaxIn "module Test\nlet ``two words`` = 1\nlet ``type`` = 2\nlet f () = ``two words`` + ``type``"
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.Backticks)
    )

[<Fact>]
let ``a hole-free interpolated string flattens`` () =
    assertSyntaxFix
        RedundantSyntax.Kind.HoleFreeInterpolation
        "module Test\nlet s = $\"just text\""
        "module Test\nlet s = \"just text\""

[<Fact>]
let ``escaped braces keep the interpolation`` () =
    Assert.Empty(
        syntaxIn "module Test\nlet s = $\"a {{ b }}\""
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.HoleFreeInterpolation)
    )

// ---- FR0085 RedundantNew ----

let private newsIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    RedundantNew.find tree sourceText checkResults

[<Fact>]
let ``new on a non-disposable construction is noted`` () =
    match newsIn "module Test\nlet sb = new System.Text.StringBuilder()" with
    | [ s ] ->
        Assert.Equal("StringBuilder", s.TypeName)

        let patched =
            applyEdit "module Test\nlet sb = new System.Text.StringBuilder()" s.Range ""

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one redundant-new fix, got %A" other

[<Fact>]
let ``new on a disposable stays`` () =
    Assert.Empty(
        newsIn
            "module Test\nopen System.IO\nlet f (p: string) =\n    use s = new FileStream(p, FileMode.Open)\n    s.ReadByte()"
    )

// ---- FR0087-FR0089 PatternCleanups ----

let private cleanupsIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    PatternCleanups.find tree sourceText checkResults

[<Fact>]
let ``cons of empty is a one-element list pattern`` () =
    let conses, _, _ =
        cleanupsIn "module Test\nlet f (xs: int list) =\n    match xs with\n    | x :: [] -> x\n    | _ -> 0"

    match conses with
    | [ s ] -> Assert.Equal("[ x ]", s.ReplacementText)
    | other -> failwithf "Expected exactly one cons fix, got %A" other

[<Fact>]
let ``all-wildcard case fields collapse`` () =
    let _, wilds, _ =
        cleanupsIn
            "module Test\ntype T =\n    | Pair of int * int\n    | One\nlet f (t: T) =\n    match t with\n    | Pair(_, _) -> 1\n    | One -> 0"

    match wilds with
    | [ s ] ->
        Assert.Equal("Pair", s.CaseName)
        Assert.Equal(" _", s.ReplacementText)
    | other -> failwithf "Expected exactly one wild-fields fix, got %A" other

[<Fact>]
let ``a partially bound case keeps its fields`` () =
    let _, wilds, _ =
        cleanupsIn
            "module Test\ntype T =\n    | Pair of int * int\n    | One\nlet f (t: T) =\n    match t with\n    | Pair(a, _) -> a\n    | One -> 0"

    Assert.Empty wilds

[<Fact>]
let ``a tuple filling a list literal is noted`` () =
    let _, _, tuples = cleanupsIn "module Test\nlet xs: (int * int) list = [ 1, 2 ]"

    match tuples with
    | [ s ] -> Assert.Equal(2, s.Elements)
    | other -> failwithf "Expected exactly one tuple-in-list note, got %A" other

[<Fact>]
let ``a semicolon list is fine`` () =
    let _, _, tuples = cleanupsIn "module Test\nlet xs = [ 1; 2 ]"
    Assert.Empty tuples

[<Fact>]
let ``prose around slashes is not a path`` () =
    Assert.Empty(pathsIn "module Test\nlet f (a: string) (b: string) = a + \" / \" + b")

[<Fact>]
let ``source-directory concatenation is a path`` () =
    match pathsIn "module Test\nlet data = __SOURCE_DIRECTORY__ + \"/data\" + \"/set.json\"" with
    | [ s ] -> Assert.Equal("/", s.Separator)
    | other -> failwithf "Expected exactly one source-dir note, got %A" other

[<Fact>]
let ``a Literal binding cannot call Path Combine`` () =
    Assert.Empty(pathsIn "module Test\n[<Literal>]\nlet DataDir = __SOURCE_DIRECTORY__ + \"/data\" + \"/set.json\"")

[<Fact>]
let ``an attribute argument cannot call Path Combine`` () =
    Assert.Empty(
        pathsIn "module Test\nopen System\n[<Obsolete(__SOURCE_DIRECTORY__ + \"/moved\" + \"/here.fs\")>]\nlet f () = 1"
    )

[<Fact>]
let ``an expression tuple list is deliberate`` () =
    let _, _, tuples =
        cleanupsIn "module Test\nlet edits (r: int) (t: string) = [ r, t, \"code\" ]"

    Assert.Empty tuples

// ---- release-review regressions ----

[<Fact>]
let ``escaped percents keep the interpolation`` () =
    // `%%` is an escaped percent in interpolated strings, a literal
    // double-percent in plain ones
    Assert.Empty(
        syntaxIn "module Test\nlet s = $\"100%%\""
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.HoleFreeInterpolation)
    )

[<Fact>]
let ``an underscore binder keeps its backticks`` () =
    // bare _ is the wildcard, not a binder
    Assert.Empty(
        syntaxIn "module Test\nlet ``_`` = 1\nlet f () = ``_`` + 1"
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.Backticks)
    )

[<Fact>]
let ``a same-file type under the short name blocks the suffix trim`` () =
    // [<My>] would resolve to type My, not MyAttribute
    Assert.Empty(
        syntaxIn
            "module Test\ntype My() = class end\ntype MyAttribute() =\n    inherit System.Attribute()\n\n[<MyAttribute>]\nlet f () = 1"
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.AttributeSuffix)
    )

[<Fact>]
let ``spaced names and backticks inside strings are untouched`` () =
    // detection is AST-ident-based: string CONTENT is invisible, and a
    // multi-word name is not a plain identifier
    Assert.Empty(
        syntaxIn "module Test\nlet ``yes fsharp supports long variables like this`` = \" `` \""
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.Backticks)
    )

// ---- FR0092 FailwithContext ----

let private failwithContextIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    FailwithContext.find tree sourceText checkResults

let private assertFailwithContext (source: string) (expectedPatched: string) =
    match failwithContextIn source with
    | [ s ] ->
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.Equal(expectedPatched, patched)
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one failwith-context hint, got %A" other

[<Fact>]
let ``static failwith message gains the enclosing arguments`` () =
    assertFailwithContext
        "module Test\nlet mymethod x =\n    failwith \"Error\""
        "module Test\nlet mymethod x =\n    failwith $\"Error, calling mymethod with x: {x}\""

[<Fact>]
let ``every parameter is reported in order`` () =
    assertFailwithContext
        "module Test\nlet locate (name: string) (index: int) =\n    failwith \"Not found\""
        "module Test\nlet locate (name: string) (index: int) =\n    failwith $\"Not found, calling locate with name: {name}, index: {index}\""

[<Fact>]
let ``the innermost enclosing function wins`` () =
    assertFailwithContext
        "module Test\nlet outer a =\n    let inner b =\n        failwith \"Bad\"\n    inner a"
        "module Test\nlet outer a =\n    let inner b =\n        failwith $\"Bad, calling inner with b: {b}\"\n    inner a"

[<Fact>]
let ``an already interpolated message is left to its author`` () =
    Assert.Empty(failwithContextIn "module Test\nlet mymethod x =\n    failwith $\"Error {x}\"")

[<Fact>]
let ``a message already naming a parameter is left alone`` () =
    Assert.Empty(failwithContextIn "module Test\nlet mymethod count =\n    failwith \"count must be positive\"")

[<Fact>]
let ``a parameterless function has nothing to report`` () =
    Assert.Empty(failwithContextIn "module Test\nlet mymethod () =\n    failwith \"Error\"")

[<Fact>]
let ``a top-level failwith outside any function is left alone`` () =
    Assert.Empty(failwithContextIn "module Test\nlet value = failwith \"Error\"")

[<Fact>]
let ``braces would need escaping so the message is left alone`` () =
    Assert.Empty(failwithContextIn "module Test\nlet mymethod x =\n    failwith \"Bad {shape}\"")

[<Fact>]
let ``a percent sign would change meaning when interpolated`` () =
    Assert.Empty(failwithContextIn "module Test\nlet mymethod x =\n    failwith \"Over 100% used\"")

[<Fact>]
let ``a shadowed failwith is not ours to rewrite`` () =
    Assert.Empty(
        failwithContextIn "module Test\nlet failwith (s: string) = ()\nlet mymethod x =\n    failwith \"Error\""
    )

[<Fact>]
let ``wildcard parameters carry nothing to report`` () =
    Assert.Empty(failwithContextIn "module Test\nlet mymethod _ =\n    failwith \"Error\"")

[<Fact>]
let ``a File factory result leaks like a bare constructor`` () =
    // File.OpenRead is THE way to open a file; ownership transfers to the
    // caller exactly as with `new FileStream(...)`
    let suggestions =
        useBindingsIn
            "let f (path: string) =\n    let stream = System.IO.File.OpenRead path\n    let n = stream.ReadByte()\n    n"

    match suggestions with
    | [ s ] ->
        Assert.Equal("stream", s.Name)
        Assert.True s.Fix.IsSome
    | other -> failwithf "Expected exactly one use-binding suggestion, got %A" other

[<Fact>]
let ``ignore applied directly still finds the map`` () =
    let suggestions =
        mapIgnoresIn "let f (xs: int list) = ignore (xs |> List.map string)"

    match suggestions with
    | [ s ] -> Assert.Equal(Some "xs |> List.iter (string >> ignore)", s.ReplacementText)
    | other -> failwithf "Expected exactly one map-ignore suggestion, got %A" other

[<Fact>]
let ``the direct Seq map spelling is the lazy nothing-runs bug too`` () =
    let suggestions = mapIgnoresIn "let f (xs: seq<int>) = Seq.map string xs |> ignore"

    match suggestions with
    | [ s ] -> Assert.Equal(None, s.ReplacementText)
    | other -> failwithf "Expected exactly one seq map-ignore note, got %A" other
