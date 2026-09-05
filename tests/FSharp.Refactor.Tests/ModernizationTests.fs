module FSharp.Refactor.Tests.ModernizationTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing
open FSharp.Compiler.Syntax

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

[<Fact>]
let ``a field named after the module holding its type keeps the nested form`` () =
    // Nu's Kasino: `Settings: Settings.GameSettings` — the field shares its
    // name with the MODULE its type lives in. `{ menu with Settings.X = v }`
    // resolves Settings as the module and the record as GameSettings: "This
    // expression was expected to have type 'Menu' but here has type
    // 'Settings.GameSettings'", rolled back five times over
    Assert.Empty(
        nestedIn
            "module Test
module Settings =
    type GameSettings = { RandomCardBacks: bool }
type Menu = { Settings: Settings.GameSettings; N: int }
let f (menu: Menu) = { menu with Settings = { menu.Settings with RandomCardBacks = true } }"
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
    | [ s ] ->
        Assert.Equal(None, s.Fix)
        Assert.Equal(Some "sink", s.Destination)
    | other -> failwithf "Expected exactly one advisory, got %A" other

[<Fact>]
let ``a disposable piped to a function names the function`` () =
    match
        useBindingsIn
            "module Test\nopen System.IO\nlet handOff (sink: FileStream -> unit) (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    stream |> sink"
    with
    | [ s ] ->
        Assert.Equal(None, s.Fix)
        Assert.Equal(Some "sink", s.Destination)
    | other -> failwithf "Expected exactly one advisory, got %A" other

[<Fact>]
let ``a disposable returned to the caller is the caller's to dispose`` () =
    // `use` here would dispose the stream before the caller ever saw it,
    // and the caller is the one that should write `use`: the factory
    // pattern is not a leak, so nothing is said
    Assert.Empty(
        useBindingsIn
            "module Test\nopen System.IO\nlet openStream (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    stream"
    )

[<Fact>]
let ``a disposable handed to a returned wrapper is adopted`` () =
    // StreamReader takes ownership and outlives this scope
    Assert.Empty(
        useBindingsIn
            "module Test\nopen System.IO\nlet openReader (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    new StreamReader(stream)"
    )

[<Fact>]
let ``a handler chained into an HttpClient is adopted, named arguments and all`` () =
    // HttpClient disposes its handler; the chain is the ClearBank/Carmel
    // pattern that used to draw two notes per client
    Assert.Empty(
        useBindingsIn
            "module Test\nopen System.Net.Http\nlet make (url: string) =\n    let handler = new HttpClientHandler(UseCookies = false)\n    new HttpClient(handler, true, BaseAddress = System.Uri url)"
    )

[<Fact>]
let ``a disposable compared but never passed still becomes a use binding`` () =
    match
        useBindingsIn
            "module Test\nopen System.IO\nlet read (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    let b = if stream = null then 0 else stream.ReadByte()
    b"
    with
    | [ s ] -> Assert.Equal(Some("let", "use"), s.Fix)
    | other -> failwithf "Expected exactly one use-binding fix, got %A" other

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
    RedundantSyntax.find None tree sourceText

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

[<Fact>]
let ``modern indexer syntax is not a single-tuple list`` () =
    // `grid[0, 1, 2]` is INDEXING. Since F# 6 it parses as an atomic
    // application of a bracket literal — the same shape as `[ 0, 1, 2 ]` —
    // and TorchSharp code is nothing but this (6 false notes in Fuuga)
    let _, _, tuples =
        cleanupsIn
            "module Test\nlet grid = Array3D.zeroCreate<int> 3 3 3\nlet read = grid[0, 1, 2]\nlet write () = grid[0, 1, 2] <- 5"

    Assert.Empty tuples

[<Fact>]
let ``the legacy dot-bracket indexer is not a single-tuple list either`` () =
    let _, _, tuples =
        cleanupsIn "module Test\nlet grid = Array3D.zeroCreate<int> 3 3 3\nlet read = grid.[0, 1, 2]"

    Assert.Empty tuples

[<Fact>]
let ``a genuine single-tuple list still fires`` () =
    // the paste trap the rule exists for: `,` where `;` was meant
    let _, _, tuples = cleanupsIn "module Test\nlet trap = [ 1, 2 ]"

    match tuples with
    | [ s ] -> Assert.Equal(2, s.Elements)
    | other -> failwithf "Expected exactly one single-tuple note, got %A" other

[<Fact>]
let ``a spaced list ARGUMENT is still a literal, not an index`` () =
    // `f [ 1, 2 ]` with a space is a real argument (NonAtomic) — the trap
    // is just as real there, so the index gate must not swallow it
    let _, _, tuples =
        cleanupsIn "module Test\nlet f (xs: (int * int) list) = xs.Length\nlet n = f [ 1, 2 ]"

    match tuples with
    | [ s ] -> Assert.Equal(2, s.Elements)
    | other -> failwithf "Expected exactly one single-tuple note, got %A" other

[<Fact>]
let ``new stays where a union case would capture the construction`` () =
    // in expression position a UNION CASE wins over a type name, so `new` is
    // the only thing forcing the constructor path. Nu's OpenGL.Texture
    // declares a LazyTexture class beside a Texture.LazyTexture case:
    // dropping `new` made a six-argument construction into a one-argument
    // case application, and the tuple was checked against the case payload
    let source =
        "module Test\ntype Thing(a: int, b: int) =\n    member _.Sum = a + b\n\ntype Wrapper =\n    | Thing of Thing\n\nlet shadowed = new Thing(1, 2)"

    Assert.Empty(newsIn source)

[<Fact>]
let ``new is still dropped where nothing shadows the type`` () =
    // the guard must not cost the ordinary case
    match newsIn "module Test\nlet sb = new System.Text.StringBuilder()" with
    | [ s ] -> Assert.Equal("StringBuilder", s.TypeName)
    | other -> failwithf "Expected the plain construction, got %A" other

[<Fact>]
let ``new is dropped where one assembly holds the name at both arities`` () =
    // .NET overloads type names by arity, and a namespace is one fragment per
    // assembly. Within ONE fragment F# picks by arity, so the bare `Resp()`
    // compiles, as `TaskCompletionSource()` and `Lazy<int>(...)` do. The hazard
    // is a name SPLIT across fragments: Microsoft.Extensions.AI.Abstractions
    // holds `ChatResponse`, Microsoft.Extensions.AI holds `ChatResponse<'T>`,
    // and Fuuga's Eval failed with "takes 2 argument(s) but is here given 0".
    // A single compilation cannot stage that; the guard counts fragments, so
    // this fixture must NOT be declined
    let source =
        "namespace Test

type Resp() =
    member _.X = 1

type Resp<'a>(a: 'a, b: int) =
    member _.A = a

module M =
    let r = new Resp()"

    match newsIn source with
    | [ s ] ->
        let patched = applyEdit source s.Range ""

        Assert.True(
            typechecksCleanly patched,
            $"Patched source does not typecheck:
%s{patched}"
        )
    | other -> failwithf "Expected the one-fragment construction to qualify, got %A" other

// ---- FR0086 and an expected FormattableString ----

let private holeFreeIn (source: string) =
    syntaxIn source
    |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.HoleFreeInterpolation)

[<Fact>]
let ``a hole-free interpolation annotated as FormattableString keeps its dollar`` () =
    // the `$` is what makes the conversion to FormattableString available; a
    // plain string never converts (Fable's StringTests)
    Assert.Empty(holeFreeIn "module Test\nlet s3: System.FormattableString = $\"I have no holes\"")

[<Fact>]
let ``a hole-free interpolation passed to a method keeps its dollar`` () =
    // only the typed tree could say whether the parameter is a
    // FormattableString; a syntactic rule declines rather than guess
    Assert.Empty(holeFreeIn "module Test\nlet s = System.FormattableString.Invariant($\"no holes\")")

[<Fact>]
let ``a hole-free interpolation bound plainly still loses its dollar`` () =
    match holeFreeIn "module Test\nlet s = $\"no holes\"" with
    | [ s ] -> Assert.Equal("\"no holes\"", s.ReplacementText)
    | other -> failwithf "Expected the plain case to keep its fix, got %A" other

[<Fact>]
let ``a hole-free interpolation passed to an F# function keeps its dollar`` () =
    // Ionide's `Log.setMessageI $"..."` takes a FormattableString and its
    // spelling does not say so (FsAutoComplete's AdaptiveServerState)
    Assert.Empty(
        holeFreeIn
            "module Test\nlet setMessageI (m: System.FormattableString) = m.Format\nlet s = setMessageI $\"Enter loading projects\""
    )

[<Fact>]
let ``FR0092 leaves a message the file reads back elsewhere`` () =
    // the test below asserts on the exact text (Fuuga): amending it breaks
    // the assertion
    Assert.Empty(
        failwithContextIn
            "module Test\nlet gen (prompt: string) =\n    failwith \"model inference failed\"\nlet check () =\n    try gen \"q\" with e -> e.Message = \"model inference failed\""
    )

[<Fact>]
let ``FR0086 strips the dollar from a printfn argument when the typed tree shows no FormattableString`` () =
    let source = "module Test\nlet f () =\n    printfn $\"Status: Processing\""
    let tree, sourceText, checkResults = parseAndCheck source

    let holeFree =
        RedundantSyntax.find (Some checkResults) tree sourceText
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.HoleFreeInterpolation)

    match holeFree with
    | [ s ] -> Assert.Equal("\"Status: Processing\"", s.ReplacementText)
    | other -> failwithf "Expected one hole-free interpolation, got %A" other

[<Fact>]
let ``FR0086 keeps the dollar for a callee taking a FormattableString`` () =
    let source =
        "module Test\nlet log (m: System.FormattableString) = m.Format\nlet f () =\n    log $\"Status: Processing\""

    let tree, sourceText, checkResults = parseAndCheck source

    Assert.Empty(
        RedundantSyntax.find (Some checkResults) tree sourceText
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.HoleFreeInterpolation)
    )

[<Fact>]
let ``FR0086 keeps the dollar in an argument without the typed tree`` () =
    let source = "module Test\nlet f () =\n    printfn $\"Status: Processing\""
    let tree, sourceText = parse source

    Assert.Empty(
        RedundantSyntax.find None tree sourceText
        |> List.filter (fun s -> s.Kind = RedundantSyntax.Kind.HoleFreeInterpolation)
    )

[<Fact>]
let ``FR0077 also offers stubs returning the empty value of each member's type`` () =
    let source =
        "module Test\ntype IThing =\n    abstract member Go: unit -> int\n    abstract member Stop: string -> unit\n    abstract member Tags: string list\n    abstract member Name: string\n\nlet t =\n    { new IThing with\n        member _.Go() = 1 }"

    match missingIn source with
    | [ s ] ->
        Assert.Contains("member _.Stop(arg0) = ()", s.EmptyInsertText)
        Assert.Contains("member _.Tags = []", s.EmptyInsertText)
        Assert.Contains("member _.Name = \"\"", s.EmptyInsertText)
        Assert.DoesNotContain("NotImplementedException", s.EmptyInsertText)
        let patched = applyEdit source s.Range s.EmptyInsertText
        Assert.True(typechecksCleanly patched, $"Empty-value stubs do not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one implement-missing fix, got %A" other

[<Fact>]
let ``FR0081: a dot-segment prefix is relative-path notation, not a join`` () =
    // Fable's `"./" + path` — Path.Combine cannot spell a `./` prefix
    Assert.Empty(pathsIn "module Test\nlet relative (path: string) = \"./\" + path")

[<Fact>]
let ``FR0081: appending parent segments is not a join either`` () =
    Assert.Empty(pathsIn "module Test\nlet up (prefix: string) = prefix + \"../\"")

[<Fact>]
let ``FR0081: a document pointer joined in a JSON module is not a filesystem path`` () =
    // FSharp.Data's JsonRuntime: `doc.Path() + "/" + name` is a JSON pointer
    Assert.Empty(pathsIn "module JsonRuntime\nlet pointer (jsonPath: string) (name: string) = jsonPath + \"/\" + name")

[<Fact>]
let ``FR0079: the editor fix is the one element itself`` () =
    match singlesIn "module Test\nopen System.Threading.Tasks\nlet run (t: Task<int>) = Task.WhenAll [| t |]" with
    | [ s ] ->
        match s.Fix with
        | Some(_, original, replacement) ->
            Assert.Equal("Task.WhenAll [| t |]", original)
            Assert.Equal("t", replacement)
        | None -> failwith "Expected the unwrap offer"
    | other -> failwithf "Expected one single-awaitable finding, got %A" other

[<Fact>]
let ``FR0089: the editor fix separates the elements with semicolons`` () =
    let _, _, tuples =
        cleanupsIn "module Test\nlet xs = [ 1, 2, 3 ]\nlet ys = [| 1.5, 2.5 |]"

    match tuples with
    | [ a; b ] ->
        let _, _, ra = a.Fix
        let _, _, rb = b.Fix
        Assert.Equal("[ 1; 2; 3 ]", ra)
        Assert.Equal("[| 1.5; 2.5 |]", rb)
    | other -> failwithf "Expected two tuple-in-list findings, got %A" other

// ---- FR0147 QualifiedNames ----

// the tight thresholds, so the shapes stay small; the defaults are 6 and 4
let private qualifiedIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    QualifiedNames.find 3 2 tree sourceText checkResults

[<Fact>]
let ``FR0147: a namespace spelled three times becomes an open after the existing opens`` () =
    let source =
        "module Test\nopen System\nlet a = System.Threading.Tasks.Task.FromResult 1\nlet b = System.Threading.Tasks.Task.Delay 10\nlet c (t: System.Threading.Tasks.Task<int>) = t.Result"

    match qualifiedIn source with
    | [ s ] ->
        Assert.Equal("System.Threading.Tasks", s.Namespace)
        Assert.Equal(3, s.Uses)
        let patched = applyAll source s.Edits

        Assert.Equal(
            "module Test\nopen System\nopen System.Threading.Tasks\nlet a = Task.FromResult 1\nlet b = Task.Delay 10\nlet c (t: Task<int>) = t.Result",
            patched
        )

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one qualified-names finding, got %A" other

[<Fact>]
let ``FR0147: only the namespace part goes, a type stays qualified by its name`` () =
    let source =
        "module Test\nlet a (p: string) = System.IO.File.Exists p\nlet b (p: string) = System.IO.File.ReadAllText p\nlet c (p: string) = System.IO.Path.GetFileName p"

    match qualifiedIn source with
    | [ s ] ->
        Assert.Equal("System.IO", s.Namespace)
        let patched = applyAll source s.Edits
        Assert.Contains("open System.IO\nlet a (p: string) = File.Exists p", patched)
        Assert.Contains("Path.GetFileName p", patched)
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one qualified-names finding, got %A" other

[<Fact>]
let ``FR0147: two uses of a shallow namespace are not worth an open`` () =
    Assert.Empty(
        qualifiedIn
            "module Test\nlet a (p: string) = System.IO.File.Exists p\nlet b (p: string) = System.IO.File.ReadAllText p"
    )

[<Fact>]
let ``FR0147: a namespace the file already opens only gets its uses shortened`` () =
    let source =
        "module Test\nopen System.IO\nlet a (p: string) = System.IO.File.Exists p"

    match qualifiedIn source with
    | [ s ] ->
        let patched = applyAll source s.Edits
        Assert.Equal("module Test\nopen System.IO\nlet a (p: string) = File.Exists p", patched)
    | other -> failwithf "Expected one shortening, got %A" other

[<Fact>]
let ``FR0147: a namespace whose open would clash with a name the file defines is noted, not fixed`` () =
    // the file's own `File` is why the author qualified System.IO.File
    let source =
        "module Test\ntype File = { Name: string }\nlet a (p: string) = System.IO.File.Exists p\nlet b (p: string) = System.IO.File.ReadAllText p\nlet c (p: string) = System.IO.Path.GetFileName p"

    match qualifiedIn source with
    | [ s ] ->
        Assert.Equal("System.IO", s.Namespace)
        Assert.Empty s.Edits
    | other -> failwithf "Expected one clash note, got %A" other

[<Fact>]
let ``FR0147: the default thresholds are six uses, or four for a deep namespace`` () =
    let tree, sourceText, checkResults =
        parseAndCheck
            "module Test\nlet a = System.Threading.Tasks.Task.FromResult 1\nlet b = System.Threading.Tasks.Task.FromResult 2\nlet c = System.Threading.Tasks.Task.FromResult 3\nlet d (p: string) = System.IO.File.Exists p\nlet e (p: string) = System.IO.File.Exists p\nlet f (p: string) = System.IO.File.Exists p\nlet g (p: string) = System.IO.File.Exists p\nlet h (p: string) = System.IO.File.Exists p"

    // three deep uses and five shallow ones: neither reaches the default
    Assert.Empty(QualifiedNames.find 6 4 tree sourceText checkResults)

    let tree2, sourceText2, checkResults2 =
        parseAndCheck
            "module Test\nlet a = System.Threading.Tasks.Task.FromResult 1\nlet b = System.Threading.Tasks.Task.FromResult 2\nlet c = System.Threading.Tasks.Task.FromResult 3\nlet d = System.Threading.Tasks.Task.FromResult 4"

    match QualifiedNames.find 6 4 tree2 sourceText2 checkResults2 with
    | [ s ] -> Assert.Equal(4, s.Uses)
    | other -> failwithf "Expected the deep namespace at four uses, got %A" other

[<Fact>]
let ``FR0147: the open lands beside the opens of the same family`` () =
    let source =
        "module Test\nopen System\nopen System.IO\nopen Microsoft.FSharp.Collections\nlet a = System.Threading.Tasks.Task.FromResult 1\nlet b = System.Threading.Tasks.Task.Delay 10\nlet c (t: System.Threading.Tasks.Task<int>) = t.Result"

    match qualifiedIn source with
    | [ s ] ->
        let patched = applyAll source s.Edits

        Assert.StartsWith(
            "module Test\nopen System\nopen System.IO\nopen System.Threading.Tasks\nopen Microsoft.FSharp.Collections\n",
            patched
        )
    | other -> failwithf "Expected one qualified-names finding, got %A" other

[<Fact>]
let ``FR0147: namespaces come deepest first, and their opens end up shallow to deep`` () =
    let source =
        "module Test\nlet a (s: string) = System.String.IsNullOrEmpty s\nlet b (s: string) = System.String.IsNullOrEmpty s\nlet c (s: string) = System.String.IsNullOrEmpty s\nlet d = System.Collections.Generic.List<int>()\nlet e = System.Collections.Generic.Dictionary<int, int>()"

    match qualifiedIn source with
    | [ deep; shallow ] ->
        Assert.Equal("System.Collections.Generic", deep.Namespace)
        Assert.Equal("System", shallow.Namespace)
        // each use belongs to exactly one namespace: no removal overlaps
        let removals = (deep.Edits @ shallow.Edits) |> List.filter (fun (_, _, r) -> r = "")
        Assert.Equal(5, removals.Length)
        // applied deep first (the order emitted), the shallow open lands above
        let patched = applyAll source (deep.Edits @ shallow.Edits)

        Assert.StartsWith(
            "module Test\nopen System\nopen System.Collections.Generic\nlet a (s: string) = String.IsNullOrEmpty s",
            patched
        )

        Assert.Contains("let d = List<int>()", patched)
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected the deep namespace first and the shallow one second, got %A" other

[<Fact>]
let ``FR0147: an open the file already has is never inserted again`` () =
    let source =
        "module Test\nopen System.Collections.Generic\nlet d = System.Collections.Generic.List<int>()\nlet e = System.Collections.Generic.Dictionary<int, int>()"

    match qualifiedIn source with
    | [ s ] ->
        Assert.Empty(s.Edits |> List.filter (fun (_, _, r) -> r.StartsWith "open"))
        let patched = applyAll source s.Edits

        Assert.Equal(
            "module Test\nopen System.Collections.Generic\nlet d = List<int>()\nlet e = Dictionary<int, int>()",
            patched
        )
    | other -> failwithf "Expected one shortening, got %A" other

[<Fact>]
let ``FR0147: an existing open System.Collections gets Generic after it and System before it`` () =
    let source =
        "module Test\nopen System.Collections\nlet a (s: string) = System.String.IsNullOrEmpty s\nlet b (s: string) = System.String.IsNullOrEmpty s\nlet c (s: string) = System.String.IsNullOrEmpty s\nlet d = System.Collections.Generic.List<int>()\nlet e = System.Collections.Generic.Dictionary<int, int>()"

    match qualifiedIn source with
    | [ deep; shallow ] ->
        let deepOpen =
            deep.Edits
            |> List.pick (fun (r, _, t) -> if t.StartsWith "open" then Some r else None)

        let shallowOpen =
            shallow.Edits
            |> List.pick (fun (r, _, t) -> if t.StartsWith "open" then Some r else None)
        // line 2 is `open System.Collections`: Generic goes below it, System above it
        Assert.Equal(3, deepOpen.StartLine)
        Assert.Equal(2, shallowOpen.StartLine)
    | other -> failwithf "Expected the deep and the shallow namespace, got %A" other

// ---- FR0075: a same-file callee is read one hop ----

[<Fact>]
let ``a disposable handed to a same-file function that disposes it is adopted`` () =
    Assert.Empty(
        useBindingsIn
            "module Test\nopen System.IO\nlet private consume (s: Stream) =\n    use s = s\n    s.ReadByte()\nlet read (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    consume stream"
    )

[<Fact>]
let ``a disposable handed to a same-file function that keeps it names the leak`` () =
    match
        useBindingsIn
            "module Test\nopen System.IO\nlet private consume (s: Stream) = s.ReadByte()\nlet read (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    consume stream"
    with
    | [ s ] ->
        Assert.Equal(None, s.Fix)
        Assert.Equal(Some "consume", s.Destination)
        Assert.True s.DestinationInspected
    | other -> failwithf "Expected exactly one advisory, got %A" other

[<Fact>]
let ``a disposable in a tuple element is followed to the matching parameter`` () =
    Assert.Empty(
        useBindingsIn
            "module Test\nopen System.IO\nlet private consume (name: string, s: Stream) =\n    s.Dispose()\n    name\nlet read (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    consume (\"x\", stream)"
    )

[<Fact>]
let ``a leak in the entry point says so`` () =
    match
        useBindingsIn
            "module Test\nopen System.IO\n[<EntryPoint>]\nlet main (argv: string[]) =\n    let stream = new FileStream(argv.[0], FileMode.Open)\n    printfn \"%d\" (stream.ReadByte())\n    0"
    with
    | [ s ] ->
        Assert.Equal(Some("let", "use"), s.Fix)
        Assert.Equal(Some UseBinding.ScopeContext.EntryPoint, s.Context)
    | other -> failwithf "Expected exactly one use-binding fix, got %A" other

[<Fact>]
let ``a leaked handle in the entry point is an ordinary leak`` () =
    // the OS reclaims the handle at exit; there is no unflushed work
    match
        useBindingsIn
            "module Test\n[<EntryPoint>]\nlet main (argv: string[]) =\n    let cts = new System.Threading.CancellationTokenSource()\n    printfn \"%b\" cts.IsCancellationRequested\n    0"
    with
    | [ s ] -> Assert.Equal(None, s.Context)
    | other -> failwithf "Expected exactly one use-binding fix, got %A" other

[<Fact>]
let ``a leak in an action method is a per-request leak`` () =
    match
        useBindingsIn
            "module Test\nopen System.IO\ntype HttpGetAttribute() =\n    inherit System.Attribute()\ntype Api() =\n    [<HttpGet>]\n    member _.Get(path: string) =\n        let stream = new FileStream(path, FileMode.Open)\n        stream.ReadByte()"
    with
    | [ s ] -> Assert.Equal(Some UseBinding.ScopeContext.RequestHandler, s.Context)
    | other -> failwithf "Expected exactly one use-binding fix, got %A" other

[<Fact>]
let ``a leak in a controller member is a per-request leak`` () =
    match
        useBindingsIn
            "module Test\nopen System.IO\ntype ControllerBase() =\n    class\n    end\ntype Api() =\n    inherit ControllerBase()\n    member _.Get(path: string) =\n        let stream = new FileStream(path, FileMode.Open)\n        stream.ReadByte()"
    with
    | [ s ] -> Assert.Equal(Some UseBinding.ScopeContext.RequestHandler, s.Context)
    | other -> failwithf "Expected exactly one use-binding fix, got %A" other

[<Fact>]
let ``FR0147: a file without a module line gets the open before its first declaration`` () =
    // the implicit module of a last file or a script has no header line to
    // go under; the "header" range is the first declaration's own line
    let source =
        "let a = System.Threading.Tasks.Task.FromResult 1\nlet b = System.Threading.Tasks.Task.Delay 10\nlet c (t: System.Threading.Tasks.Task<int>) = t.Result"

    match qualifiedIn source with
    | [ s ] ->
        let patched = applyAll source s.Edits

        Assert.Equal(
            "open System.Threading.Tasks\nlet a = Task.FromResult 1\nlet b = Task.Delay 10\nlet c (t: Task<int>) = t.Result",
            patched
        )

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one qualified-names finding, got %A" other

[<Fact>]
let ``FR0147: an expression head naming a type from elsewhere is a clash, a module of that name is not`` () =
    // `Queue.Synchronized` is System.Collections.Queue; opening
    // System.Collections.Generic would re-bind `Queue` to the generic one
    let clashing =
        "module Test\nopen System.Collections\nlet q () = Queue.Synchronized(Queue())\nlet a = System.Collections.Generic.List<int>()\nlet b = System.Collections.Generic.List<int>()\nlet c = System.Collections.Generic.List<int>()"

    match qualifiedIn clashing with
    | [ s ] ->
        Assert.Equal("System.Collections.Generic", s.Namespace)
        Assert.Empty s.Edits
    | other -> failwithf "Expected one clash note, got %A" other

    // `List.map` is the F# List module, which coexists with the generic List
    let coexisting =
        "module Test\nlet xs = List.map id [ 1 ]\nlet a = System.Collections.Generic.List<int>()\nlet b = System.Collections.Generic.List<int>()\nlet c = System.Collections.Generic.List<int>()"

    match qualifiedIn coexisting with
    | [ s ] -> Assert.NotEmpty s.Edits
    | other -> failwithf "Expected one qualified-names fix, got %A" other

[<Fact>]
let ``a disposable handed to one of two same-named functions is not followed`` () =
    match
        useBindingsIn
            "module Test\nopen System.IO\nmodule A =\n    let consume (s: Stream) =\n        use s = s\n        s.ReadByte()\nmodule B =\n    let consume (s: Stream) = s.ReadByte()\nopen B\nlet read (path: string) =\n    let stream = new FileStream(path, FileMode.Open)\n    consume stream"
    with
    | [ s ] ->
        Assert.Equal(Some "consume", s.Destination)
        Assert.False s.DestinationInspected
    | other -> failwithf "Expected exactly one advisory, got %A" other

[<Fact>]
let ``FR0147: a short name another open already brings is a clash, even for a namespace that is open`` () =
    // System.Timers has a Timer too: the author qualified System.Threading's
    // on purpose (prismatic's FSharp.Data.HttpMethod beside System.Net.Http's)
    let freshOpen =
        "module Test\nopen System.Timers\nlet a (t: System.Threading.Timer) = t.Dispose()\nlet b (t: System.Threading.Timer) = t.Dispose()\nlet c (t: System.Threading.Timer) = t.Dispose()"

    match qualifiedIn freshOpen with
    | [ s ] ->
        Assert.Equal("System.Threading", s.Namespace)
        Assert.Empty s.Edits
    | other -> failwithf "Expected one clash note, got %A" other

    // already open, still qualified: nothing to say at all
    let alreadyOpen =
        "module Test\nopen System.Threading\nopen System.Timers\nlet a (t: System.Threading.Timer) = t.Dispose()\nlet b (t: System.Threading.Timer) = t.Dispose()\nlet c (t: System.Threading.Timer) = t.Dispose()"

    Assert.Empty(qualifiedIn alreadyOpen)

let private qualifiedInSecond (lib: string) (user: string) =
    let tree, sourceText, checkResults = parseAndCheckSecond lib user
    QualifiedNames.find 3 2 tree sourceText checkResults

[<Fact>]
let ``FR0147: a module named like its namespace is not the namespace`` () =
    // toro: `namespace rec Toro` holds a `module Toro`; `Toro.noGrad` names
    // the module, and under `open Toro` a bare `noGrad` reaches nothing
    let lib =
        "namespace Toro\nmodule Toro =\n    let noGrad (f: unit -> 'a) : 'a = f ()"

    let user =
        "module Example\nopen Toro\nlet a = Toro.noGrad (fun () -> 1)\nlet b = Toro.noGrad (fun () -> 2)\nlet c = Toro.noGrad (fun () -> 3)"

    Assert.Empty(qualifiedInSecond lib user)

[<Fact>]
let ``FR0147: a same-project module of the introduced name is a clash`` () =
    // FsAutoComplete: Utils.Utils.Expect beside Expecto.Expect — the
    // qualified Expecto.Expect was the author's way of reaching the other
    let lib =
        "namespace Lib\nmodule Expect =\n    let equal (a: int) (b: int) = ()\nnamespace Utils\nmodule Utils =\n    module Expect =\n        let equal (a: int) (b: int) (msg: string) = ()"

    let user =
        "module Tests\nopen Lib\nopen Utils.Utils\nlet a = Expect.equal 1 1 \"m\"\nlet b = Lib.Expect.equal 1 1\nlet c = Lib.Expect.equal 2 2\nlet d = Lib.Expect.equal 3 3"

    Assert.Empty(qualifiedInSecond lib user)

[<Fact>]
let ``FR0147: a same-project namespace still gets its open`` () =
    let lib = "namespace Lib.Deep\ntype Thing() =\n    static member Make() = Thing()"

    let user =
        "module Example\nlet a = Lib.Deep.Thing.Make()\nlet b = Lib.Deep.Thing.Make()\nlet c = Lib.Deep.Thing.Make()"

    match qualifiedInSecond lib user with
    | [ s ] ->
        Assert.Equal("Lib.Deep", s.Namespace)
        Assert.NotEmpty s.Edits
    | other -> failwithf "Expected one qualified-names fix, got %A" other

[<Fact>]
let ``FR0147: the open goes under the module line, not under the doc comment above it`` () =
    // Logari: a doc comment precedes `module Logari`, and the module's range
    // starts at the comment
    let source =
        "/// Doc line one\n/// Doc line two\nmodule Test\nlet a = System.Threading.Tasks.Task.FromResult 1\nlet b = System.Threading.Tasks.Task.Delay 10\nlet c (t: System.Threading.Tasks.Task<int>) = t.Result"

    match qualifiedIn source with
    | [ s ] ->
        let patched = applyAll source s.Edits

        Assert.Equal(
            "/// Doc line one\n/// Doc line two\nmodule Test\nopen System.Threading.Tasks\nlet a = Task.FromResult 1\nlet b = Task.Delay 10\nlet c (t: Task<int>) = t.Result",
            patched
        )

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one qualified-names finding, got %A" other

[<Fact>]
let ``FR0147: an open further down the file covers nothing above it`` () =
    // Fuuga's Eval.fs opens System.Text.RegularExpressions at line 1811; the
    // uses above it are not "already open", and the new open cannot land
    // beside that one either
    let source =
        "module Test\nlet a = System.Text.RegularExpressions.Regex(\"x\")\nlet b = System.Text.RegularExpressions.Regex(\"y\")\nlet c = System.Text.RegularExpressions.Regex(\"z\")\nopen System.Text.RegularExpressions\nlet d = Regex(\"w\")"

    match qualifiedIn source with
    | [ s ] ->
        let patched = applyAll source s.Edits

        Assert.Equal(
            "module Test\nopen System.Text.RegularExpressions\nlet a = Regex(\"x\")\nlet b = Regex(\"y\")\nlet c = Regex(\"z\")\nopen System.Text.RegularExpressions\nlet d = Regex(\"w\")",
            patched
        )

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one qualified-names finding, got %A" other

[<Fact>]
let ``FR0147: each top-level namespace block gets its own open`` () =
    // an open in `namespace A` covers nothing in the `namespace B` below it
    let block (ns: string) (name: string) =
        $"namespace {ns}\nmodule {name} =\n    let a = System.Threading.Tasks.Task.FromResult 1\n    let b = System.Threading.Tasks.Task.Delay 10\n    let c (t: System.Threading.Tasks.Task<int>) = t.Result"

    let source = block "A" "M" + "\n" + block "B" "N"

    match qualifiedIn source with
    | [ first; second ] ->
        let edits = first.Edits @ second.Edits
        let patched = applyAll source edits

        let shortened (ns: string) (name: string) =
            $"namespace {ns}\nopen System.Threading.Tasks\nmodule {name} =\n    let a = Task.FromResult 1\n    let b = Task.Delay 10\n    let c (t: Task<int>) = t.Result"

        Assert.Equal(shortened "A" "M" + "\n" + shortened "B" "N", patched)
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one finding per block, got %A" other

[<Fact>]
let ``FR0147: a namespace shadowed by a module of its name is never opened`` () =
    // Nu: `[<RequireQualifiedAccess>] module OpenGL` in namespace Nu beside
    // `namespace Nu.OpenGL` — `open Nu.OpenGL` resolves to the module and
    // is refused, so the qualified spelling stays
    let lib =
        "namespace Nu\n[<RequireQualifiedAccess>]\nmodule OpenGL =\n    let version = 1\nnamespace Nu.OpenGL\ntype Thing() =\n    static member Make() = Thing()"

    let user =
        "module Example\nlet a = Nu.OpenGL.Thing.Make()\nlet b = Nu.OpenGL.Thing.Make()\nlet c = Nu.OpenGL.Thing.Make()"

    // `open Nu` with `OpenGL.Thing` is fine (the module name still
    // qualifies the access); `open Nu.OpenGL` is what the compiler refuses
    for s in qualifiedInSecond lib user do
        for _, _, text in s.Edits do
            Assert.DoesNotContain("open Nu.OpenGL", text)

[<Fact>]
let ``FR0147: an active pattern another open brings shadows the constructor of that name`` () =
    // FsAutoComplete: `(|Ident|_|)` from an opened module over
    // FSharp.Compiler.Syntax.Ident — the shortened `Ident(...)` applies the
    // pattern ("This value is not a function")
    let lib =
        "namespace Lib.Syntax\ntype Ident(text: string) =\n    member _.Text = text\nnamespace Lib.Helpers\nmodule Patterns =\n    let (|Ident|_|) (s: string) = if s = \"\" then None else Some s"

    let user =
        "module Example\nopen Lib.Syntax\nopen Lib.Helpers.Patterns\nlet a = Lib.Syntax.Ident(\"a\")\nlet b = Lib.Syntax.Ident(\"b\")\nlet c = Lib.Syntax.Ident(\"c\")"

    Assert.Empty(qualifiedInSecond lib user)

[<Fact>]
let ``FR0147: an open inside a nested module does not count for the file`` () =
    // Fuuga's ConfigTests: nested test modules with their own opens; a
    // qualified System.IO in a later nested module still needs the open
    let source =
        "module Test\nmodule First =\n    open System.Text\n    let a = StringBuilder()\nmodule Second =\n    let p = System.IO.Path.Combine(\"a\", \"b\")\n    let q = System.IO.File.Exists p\n    let r = System.IO.File.Exists \"c\""

    match qualifiedIn source with
    | [ s ] ->
        Assert.Equal("System.IO", s.Namespace)
        let patched = applyAll source s.Edits

        Assert.Equal(
            "module Test\nopen System.IO\nmodule First =\n    open System.Text\n    let a = StringBuilder()\nmodule Second =\n    let p = Path.Combine(\"a\", \"b\")\n    let q = File.Exists p\n    let r = File.Exists \"c\"",
            patched
        )

        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one qualified-names fix, got %A" other

// ---- FR0085: a function spelled like the type keeps the `new` ----

[<Fact>]
let ``FR0085: a function bound with the type's name keeps new`` () =
    // TypeProviders SDK: `let SharedRow(elems) = new SharedRow(elems, hash)`;
    // without `new` the bare name is the function, called with the wrong arguments
    let shadowed =
        "module Test\ntype SharedRow(elems: int[], hash: int) =\n    member _.Hash = hash\nlet SharedRow(elems: int[]) = new SharedRow(elems, elems.Length)\nlet make (xs: int[]) = new SharedRow(xs, 1)"

    let tree, sourceText, checkResults = parseAndCheck shadowed
    Assert.Empty(RedundantNew.find tree sourceText checkResults)

    let plain =
        "module Test\ntype SharedRow(elems: int[], hash: int) =\n    member _.Hash = hash\nlet make (xs: int[]) = new SharedRow(xs, 1)"

    let tree, sourceText, checkResults = parseAndCheck plain
    Assert.Single(RedundantNew.find tree sourceText checkResults) |> ignore

[<Fact>]
let ``a disposable handed to a disposable owner's property or Add is adopted`` () =
    // prismatic: HttpRequestMessage disposes its Content, MultipartContent
    // its parts — the most frequent FR0075 notes there were these
    Assert.Empty(
        useBindingsIn
            "module Test\nopen System.Net.Http\nlet send (client: HttpClient) (body: string) =\n    use request = new HttpRequestMessage(HttpMethod.Post, \"http://x\")\n    let content = new StringContent(body)\n    request.Content <- content\n    client.Send request"
    )

    Assert.Empty(
        useBindingsIn
            "module Test\nopen System.Net.Http\nlet build (body: string) =\n    use multipart = new MultipartFormDataContent()\n    let part = new StringContent(body)\n    multipart.Add(part, \"body\")\n    multipart.Headers.ContentLength"
    )

[<Fact>]
let ``FR0147: a union case an F#-compiled assembly's namespace brings is a clash`` () =
    // FsAutoComplete: `type SymbolKind = | Ident | ...` in namespace
    // FsAutoComplete (FsAutoComplete.Core.dll) beside FCS's Ident class —
    // an F# assembly nests its types under namespace entities, which a
    // top-level scan never saw. FSharp.Core is such an assembly: its
    // Microsoft.FSharp.Control brings `Async`
    let lib = "namespace Lib.Ctl\ntype Async(x: int) =\n    member _.X = x"

    let user =
        "module Example\nopen Microsoft.FSharp.Control\nlet a = Lib.Ctl.Async(1).X\nlet b = Lib.Ctl.Async(2).X\nlet c = Lib.Ctl.Async(3).X"

    match qualifiedInSecond lib user with
    | [ s ] -> Assert.Empty s.Edits
    | [] -> ()
    | other -> failwithf "Expected a clash note or silence, got %A" other

// ---- FR0140: a greedy last constructor argument gets its parentheses ----

[<Fact>]
let ``FR0140: a lambda argument is parenthesised before the named properties`` () =
    // TypeProviders SDK: `TypeProviderConfig(fun _ -> false)` — appended
    // properties would land inside the lambda as a tuple
    let source =
        "module Test\ntype Cfg(f: int -> bool) =\n    member val Hosted = false with get, set\n    member val Name = \"\" with get, set\nlet make () =\n    let cfg = Cfg(fun _ -> false)\n    cfg.Hosted <- true\n    cfg.Name <- \"x\"\n    cfg"

    let tree, sourceText, checkResults = parseAndCheck source

    match ObjectInitializer.find tree sourceText checkResults with
    | [ s ] ->
        Assert.Equal("Cfg((fun _ -> false), Hosted = true, Name = \"x\")", s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected one construction rewrite, got %A" other
