# FSharp.Refactorings

ReSharper-style **functional refactoring suggestions for F#** — light-bulb quick
fixes in your editor, and a command-line tool that applies them in bulk. Built on
[FSharp.Analyzers.SDK](https://github.com/ionide/FSharp.Analyzers.SDK).

Suggestions are `Hint` severity: they mark an opportunity, not a defect, and
never gate your build.

---

# Using it

## Quick start

Nothing to configure — the tool reads your project, reports what it would
change, and only edits when you tell it to:

```bash
dotnet tool install --global fsharp-refactorings-apply
fsharp-refactor Your.fsproj --dry-run
```

That prints every fix it would make, with file and position, and writes
nothing. When the list looks right, drop the flag to apply them:

```bash
fsharp-refactor Your.fsproj
```

It refuses a compilation that does not already build, and fails loudly if applying
ever introduces an error. For light bulbs while you type, see
[VS Code / Ionide](#vs-code--ionide) below.

Point it at whatever you have — the kind is read off the path:

| | |
|---|---|
| `Your.fsproj` | one project |
| `Thing.fs` | one source file — its project is found and analysed, but only that file is edited |
| `build.fsx` | one script — no MSBuild step at all, so it starts instantly |
| `Your.sln`, `Your.slnx` | every F# project the solution lists |
| `src/` | the solution in that directory, or the projects beneath it |
| `"src/**/*.fsproj"` | everything the glob matches |

## What it changes

A spread of what the 90-odd rules do — the full list is in
[Refactorings](#refactorings):

| | Before | After |
|---|---|---|
| Correctness | `let s = new FileStream(p, m)` | `use s = new FileStream(p, m)` |
| Correctness | `raise (Exception "boom")` | `failwith "boom"` |
| Performance | `s.Contains "x"` | `s.Contains 'x'` |
| Performance | `xs \|> Seq.toList \|> List.map f` | `xs \|> Seq.map f \|> Seq.toList` |
| Idiom | `match b with \| true -> 1 \| false -> 0` | `if b then 1 else 0` |
| Redundancy | `new StringBuilder()` | `StringBuilder()` |
| Redundancy | `[<SerializableAttribute>]` | `[<Serializable>]` |
| Diagnostics | `failwith "Error"` | `failwith $"Error, calling f with x: {x}"` |

## Editor and CI setup

The analyzers ship as [`FSharp.Refactorings.Analyzers`](https://www.nuget.org/packages/FSharp.Refactorings.Analyzers).
The package is a development dependency: it only produces hints and quick
fixes — nothing from it flows into your compiled output.

NOTE: EVEN WHEN ADDING A NUGET REFERENCE, THIS ANALYSER WILL NOT COME TO OUTPUT PATH (BIN) AND WILL NOT BE PART OF YOUR PROJECT.

### VS Code / Ionide

Reference the package from the project you want analyzed:

```xml
<PackageReference Include="FSharp.Refactorings.Analyzers" Version="0.3.0" PrivateAssets="all" />
```

then point Ionide at the restored analyzers in `.vscode/settings.json`:

```json
{
  "FSharp.enableAnalyzers": true,
  "FSharp.analyzersPath": [
    "~/.nuget/packages/fsharp.refactorings.analyzers/0.3.0/analyzers/dotnet/fs"
  ]
}
```

(on Windows the cache lives under `%USERPROFILE%\.nuget\packages`). Suggestions
appear as `Hint`-severity diagnostics with a light-bulb one-click fix.

### CLI / CI

```bash
dotnet tool install --global fsharp-analyzers
fsharp-analyzers --project src/YourProject.fsproj --analyzers-path ~/.nuget/packages/fsharp.refactorings.analyzers/0.3.0/analyzers/dotnet/fs --code-root . --report analysis.sarif
```

`fsharp-analyzers` is the analyzer HOST, and it is the dotnet tool you
install. This package is not a tool: it is a library of analyzer assemblies
the host loads, so it is passed as a directory rather than installed. That
directory sits in the NuGet cache because an analyzer package deliberately
has no `lib/` folder and is marked a development dependency — a
`PackageReference` therefore puts nothing in your `bin`, and after a restore
the cache is where the assemblies live. `--analyzers-path` takes any folder
holding them and searches it recursively.

The CLI only REPORTS; it never edits your files, which is what you want in
CI (SARIF output works in GitHub code scanning). To apply the fixes, use our
own tool below. Individual rules can be turned off per repository with a
`fsharprefactorings.json` — see [Configuration](#configuration) below.

### Applying fixes from the command line

The [`fsharp-refactorings-apply`](https://www.nuget.org/packages/fsharp-refactorings-apply)
dotnet tool applies the quick fixes directly to your files:

```bash
dotnet tool install --global fsharp-refactorings-apply
fsharp-refactor Your.fsproj [--dry-run] [--codes FR0002,FR0031] [--api-changes] [--jobs 4] [--max-passes 5]
```

(or from this repository:
`dotnet run --project src/FSharp.Refactorings.Apply -c Release -- Your.fsproj ...`)

For a project it takes the exact compiler arguments from MSBuild; for a
script FCS resolves the references itself and MSBuild never runs. Either
way it then runs every analyzer, applies non-overlapping fixes bottom-up,
and re-analyzes until a pass applies nothing — a fix can enable further
fixes. It refuses a compilation that already has errors, and fails loudly
if applying ever introduces one.

| Flag | |
|---|---|
| `--dry-run` | Report only: lists every fix it would make, with file and position, and writes nothing. Rewriting is never implicit — drop the flag to let it edit. |
| `--codes FR0002,FR0031` | Restrict the run to chosen rules. |
| `--jobs <n>` | Typecheck that many files at once (default 4, clamped to 2–4 by core count). Trades CPU for wall clock; because FCS reuses each file's prefix within one incremental build, the gain peaks around 4 and reverses if pushed higher. `--jobs 1` is the sequential sweep. |
| `--framework <tfm>` | Analyse against this target framework instead of the narrowest one — see below. |
| `--max-passes <n>` | Fix-then-reanalyze iterations (default 5). |
| `--help` | The same list, from the tool itself (`-h` and `/?` also work). |
| `--api-changes` | Also apply the cross-file fixes described below. |

### Multi-targeted projects

Nothing extra to do: a multi-targeted project is worked through framework
by framework, narrowest first.

That is not busywork. A rule gated on what the target can resolve behaves
differently per framework — `s.Contains 'x'` is offered under `net8.0`,
where the char overload exists, and does not compile for a `netstandard2.0`
target that lacks it. And each framework activates its own `#if` branches,
so code behind another one's is not in the parse tree at all. One pass
could only ever see part of the code.

Narrowest first means the fixes valid everywhere land before any that suit
only a wider surface, and every pass ends by building **all** the
frameworks, so a fix that does not generalise fails loudly instead of
passing as success.

Given this, one plain `fsharp-refactor Your.fsproj` produces:

```fsharp
let has (s: string) =
#if NETSTANDARD2_0
    s.Contains("x")     // left alone: no char overload here
#else
    s.Contains('x')     // rewritten under the net8.0 pass
#endif
```

`--framework <tfm>` restricts a run to one framework if you want it.

`--api-changes` opts into rewrites that change internal or public
signatures — currying a tupled function (FR0090) and reordering its
parameters data-last (FR0091) — rewriting every call site in the project.
Without it those are held back and only counted. It also widens the
contained-type hints (FR0022, FR0069, FR0070, FR0093) to public types.
Consumers outside the project are why this is opt-in: their call sites
cannot be rewritten, so each rule only fires where a missed one would fail
to compile rather than change behaviour silently. Naming a single source
file skips these entirely — asking for one file and getting edits in its
callers would be a surprise.

## Refactorings

Roadmap based on ["F# refactoring possibilities"](https://www.slideshare.net/ThoriumT/f-refactoring-possibilities):

| Code | Refactoring | Status |
|------|-------------|--------|
| FR0001 | Boolean `match` → `if-else` | |
| FR0002 | Manual `Some/None` (and `ValueSome/ValueNone`) match → `Option`/`ValueOption` `map`/`bind`/`flatten`/`defaultValue`/`defaultWith`/`isSome`/`isNone`/`iter`/`exists`/`forall` + map-then-default combos | |
| FR0003 | Extract function composition (`f >> g`) from pipeline/nested-application lambdas | |
| FR0004 | Move `List`/`Seq`/`Array` conversion past the next pipeline operation (or drop it before consuming ops) | |
| FR0005 | Strip do-nothing CE wrapping (`async { return! c }`, rewrap identity, immediately-run wraps, `task { return x }` → `Task.FromResult`) | |
| FR0006 | Extract a `when` guard into an active pattern | |
| FR0007 | Remove `mutable` from never-mutated local bindings and type-level `let mutable` fields (class lets are private to the type, so the whole mutation scope is visible) | |
| FR0008 | Tupled → curried parameters for `private` functions (definition + all call sites) | |
| FR0009 | Manual `Ok/Error` match → `Result.map`/`bind`/`mapError`/`isOk`/`isError`/`defaultValue`/`defaultWith`/`iter` | |
| FR0010 | Simplifications: `if c then true else false` → `c`; `x = None` → `Option.isNone x`; `List.length xs = 0` → `List.isEmpty xs` (List/Seq/Array/Set/Map) | |
| FR0011 | Trivial partial active patterns → `[<return: Struct>]` `ValueSome`/`ValueNone` (perf: no allocation per match attempt) | |
| FR0012 | Term-rewriting hints (fsharplint-style `lhs ===> rhs` rules): comparison flips, `x = true`, null checks via `isNull`, map fusion, `isEmpty (filter ...)` → `exists`, `sum (map ...)` → `sumBy`, `map id`, `id >>`, `compare ... = 0`, and more — extensible per repository | |
| FR0013 | Redundant parentheses around single atomic arguments: `List.max([4; 3])` → `List.max [4; 3]` | |
| FR0014 | `ContainsKey` + indexer double lookup → single `TryGetValue` (race fix on `ConcurrentDictionary`); F# `Map` gets the `TryFind` option idiom | |
| FR0015 | Literal regex patterns → `StartsWith`/`EndsWith`/`Contains`; static `Regex` calls inside loops are hoisted to a `let private xRegex = Regex "..."` module binding (advice-only when the `open` is missing or the name is taken) | |
| FR0016 | Small value-type-only unions → `[<Struct>]` (perf: no heap allocation per value) | |
| FR0017 | `Async` discarded with `ignore` (never runs) — fix-less hint pointing at `Async.Ignore`/`Async.Start` | |
| FR0018 | Check-then-add → single `TryAdd` (race fix on `ConcurrentDictionary`, double-lookup fix on `Dictionary`) | |
| FR0019 | `Equals` override without `GetHashCode` (hash-based collections misbehave) | |
| FR0020 | Abstract member used during construction (override runs before derived init) | |
| FR0021 | Redundant `.ToString()` inside interpolated strings | |
| FR0022 | Non-public union cases with unnamed tuple fields take the field names their match sites already bind (`Line of int * decimal` → `Line of qty: int * price: decimal`); definition-only edit, positional sites stay valid | |
| FR0023 | Private two-parameter functions called as `fun x -> f x k` are reordered data-last, all in one fix: the definition swaps to `let private f k x`, direct calls swap their arguments, and the lambda — which under the new order would read `fun x -> f k x` — eta-reduces to the partial application `f k` (`List.map (fun x -> scale x 2)` ends up as `List.map (scale 2)`) | |
| FR0024 | `raise (Exception msg)` → `failwith msg` (plain `System.Exception` only — the raised type and message are unchanged) | |
| FR0025 | Null test wrapping a value into an option → `Option.ofObj` / `ValueOption.ofObj` (`if isNull x then None else Some x`, the negated and `= null` forms, and the two-clause `match x with null -> ...`; `Some`/`None` typed-gated to FSharp.Core) | |
| FR0026 | Mutable backing field + trivial get/set member → `member val X = init with get, set` (field must be untouched elsewhere in the type; pure-atom initializer) | |
| FR0027 | GC-lifetime note (no fix): a lambda that captures `this` — directly or through an instance `let` field — handed to an event/observable sink (`.Add`, `.Subscribe`, `.AddHandler`, `Observable.add`, ...) keeps the whole object alive until the handler is removed; sinks are typed-gated so collection `.Add`s never fire | |
| FR0028 | N+1 note (no fix): a `for` over an `IQueryable` nested inside another loop executes one database query per outer iteration; typed-gated so in-memory sequences never fire, and an outer loop batched with `chunkBySize` suppresses the note | |
| FR0029 | Task state-machine advice (no fix; FS3511 itself is emitted at codegen, invisible to analyzers): a `let rec` in a resumable `task { }` body is flagged always (definite dynamic-fallback producer); oversized tasks (≥8 awaits or ≥60 lines) get the applicable shrinking moves — hoist plain leading `let`s out, split an if/match whose branches each await into per-branch tasks, extract a long non-awaiting tail into a plain function | |
| FR0030 | A loop whose whole body is a single `ResizeArray.Add` becomes one `AddRange` call (`for x in xs do acc.Add(x * 2)` → `acc.AddRange(xs \|> Seq.map (fun x -> x * 2))`); `Add` is typed-gated to `List<'T>` so `HashSet.Add` never matches | |
| FR0031 | String `+` chains mixing literals and string values → interpolated string (`"Hello " + name + "!"` → `$"Hello {name}!"`); every operand must be a literal or typed-`string` identifier/path and the `+` itself must resolve to FSharp.Core, so a custom `(+)` never rewrites; literals containing `{`/`}`/`%` leave the chain alone | |
| FR0032 | A type that creates a disposable field (`let stream = new FileStream(...)`) without implementing `IDisposable` is noted (no fix); injected constructor parameters don't count — the injector owns them | |
| FR0033 | An instance member touching no instance state — no self identifier, instance `let` field, primary-constructor parameter, or `base` — can be `static member` (note only: call sites change) | |
| FR0034 | `if x.IsSome then x.Value + 1 else e` → `match x with \| Some v -> v + 1 \| None -> e` (`.Value` throws when misused; the match cannot); handles the `IsNone`/negated forms, else-less unit `if`, `x.Value.P` prefixes, and spells `ValueSome`/`ValueNone` when the receiver is a voption (typed-gated, so custom `IsSome`/`Value` members never match); boolean combos rewrite to combinators — `x.IsSome && p x.Value` → `Option.exists`, `x.IsNone \|\| p x.Value` → `Option.forall`, chains join inside the lambda | |
| FR0035 | `List/Array/Seq.contains x ys` inside a loop — or inside a callback given to a collection function — scans `ys` linearly per iteration; note suggests building a Set once outside (probing the loop variable itself never fires) | |
| FR0036 | Fragile runtime type comparisons (notes): `GetType().Name = "..."` breaks silently on renames/namespaces — compare types instead; `x.GetType() = typeof<T>` is exact-type equality — `x :? T` if subtypes are fine | |
| FR0037 | Build-once types constructed inside a loop: `ConcurrentDictionary`, `JsonSerializerOptions` (CA1869), `SearchValues.Create` (CA1870) — all expensive by design; note suggests hoisting out or making static | |
| FR0038 | Char overloads for single-character strings (CA1834/1847/1865-67): `s.Contains "x"` → `s.Contains 'x'` and `sb.Append "x"` → `sb.Append 'x'` (both ordinal already — fix); `s.StartsWith("x", StringComparison.Ordinal)` → `s.StartsWith('x')` (fix); bare `StartsWith`/`EndsWith`/`IndexOf` are culture-sensitive where the char overload is ordinal, so those get an advisory note only; receivers typed-gated to `String`/`StringBuilder` | |
| FR0039 | Allocating case-insensitive comparisons (CA1862, note): `a.ToLower() = b.ToLower()` and `s.ToLower().StartsWith "abc"` allocate lowered copies just to compare; `String.Equals(a, b, StringComparison...IgnoreCase)` / the comparison overloads are allocation-free — comparison type stays the author's deliberate choice | |
| FR0040 | Redundant membership guards (CA1853/1868, fix): `if d.ContainsKey k then d.Remove k \|> ignore` → `d.Remove k \|> ignore`, `if not (s.Contains x) then s.Add x \|> ignore` → `s.Add x \|> ignore` — the operations already return `false` on a miss; typed-gated to `Dictionary`/`HashSet`/`SortedSet` | |
| FR0041 | `Array.sum/average/min/max` on `int[]`/`int64[]` is a scalar loop; on .NET 8+ System.Linq's `Sum()`/`Average()`/`Min()`/`Max()` are SIMD-vectorized (note only: LINQ `Sum` throws on overflow where `Array.sum` wraps; floats excluded — NaN semantics differ) | |
| FR0042 | Fully applied `sprintf` → typed interpolated string (`sprintf "asdf %s" x` → `$"asdf %s{x}"`); specifiers are kept verbatim so the output is byte-identical; guards: regular literal with no `{`/`}`, simple arguments only, no `%a`/`%t`/`*`-widths, partial applications never match | |
| FR0043 | In an interpolated string that *already* has a typed hole, the remaining plain holes gain specifiers (`$"%s{name} is {age}"` → `$"%s{name} is %d{age}"`) — free compile-time type pinning since the string is on the printf path anyway; specifier-free strings are left on the F# 8 `String.Concat` fast path, and only ToString-identical specifiers are used (`%s`/`%d`/`%c`; never `%b` or `%f`) | |
| FR0044 | `raise ex` in a `with` handler resets the stack trace → `reraise ()` (CA2200, fix); skipped inside lambdas/CEs/nested trys where `reraise` would not compile or would mean a different exception | |
| FR0045 | `x = nan` / `x <> Double.NaN` never holds (IEEE 754) → `System.Double.IsNaN x` / negated (CA2242, fix); `Single.NaN` uses `Single.IsNaN` | |
| FR0046 | `lock "str"` / `lock typeof<T>` / `lock (x.GetType())` — weak-identity objects are process-wide singletons, so the monitor is shared with strangers (CA2002, note): use a dedicated `let lockObj = obj ()` | |
| FR0047 | A type implementing `IDisposable` whose `Dispose` never touches one of its `new`-constructed disposable fields (CA2213, note) — the mirror of FR0032 | |
| FR0048 | `String.Format("{0} of {1}", x)` — a placeholder without an argument throws `FormatException` at runtime (CA2241, note); `{{` escapes handled, culture-first overload ignored | |
| FR0049 | Sync-over-async (CA1849/VSTHRD): `.Result`, `.Wait()`, `GetAwaiter().GetResult()`, `Async.RunSynchronously`, `Thread.Sleep` **inside** `async`/`task { }` invite thread-pool starvation and deadlocks (typed-gated receivers; `Thread.Sleep n` gets a `do! Async.Sleep n` / `do! Task.Delay n` fix in statement position); `.Result`/`.Wait()`/`GetResult()` **outside** CEs get the boundary note — wrap in `task { }` or use the sync API (`Async.RunSynchronously` outside a CE is F#'s intended sync boundary and stays quiet) | |
| FR0050 | `let mutable total = 0` + `for x in xs do total <- total + x` → `let total = xs \|> Seq.sum` (fix); projections → `sumBy`, general combines → `Seq.fold (fun acc x -> ...) init` — same expression, same bindings, no mutable | |
| FR0051 | `acc <- acc @ [x]` / `acc <- Array.append acc [\|x\|]` inside a loop copies the accumulator per iteration — O(n²) (note): use a ResizeArray, or cons and `List.rev` | |
| FR0052 | `q.Count = 0` on `ConcurrentQueue`/`Stack`/`Bag` → `q.IsEmpty` (CA1836, fix): their `Count` walks segments, `IsEmpty` peeks | |
| FR0053 | `BitConverter.ToString(bytes).Replace("-", "")` → `System.Convert.ToHexString bytes` (CA1872, fix) | |
| FR0054 | `raise`/`failwith` inside `Equals`/`GetHashCode`/`ToString`/`Dispose` overrides (CA1065, note): implicit callers (hash containers, debuggers, formatting, finalization) never expect them to throw; raises inside the member's own `try` stay quiet | |
| FR0055 | `try ... with _ -> ()` (or `:? Exception -> ()`) swallows every exception including cancellation, and `with _ -> ""` / `0` / `Unchecked.defaultof` / `None` / `null` / `[]` additionally disguises the failure as a result (note): log or `reraise ()`, and catch the specific type; deliberately ignoring a *specific* exception stays quiet, as does the `try ...; true with _ -> false` probe idiom | |
| FR0057 | XML doc drift (note): a doc comment with `<param>` tags that misses some actual parameters — the compiler warns about *unknown* names (FS3390) but not *missing* ones; fully undocumented functions are a style choice and stay quiet | |
| FR0058 | A `let rec` re-entering itself through `seq`/`taskSeq`/`asyncSeq { }` builds a fresh enumerator per recursion level — every element pays O(depth) `MoveNext`s (note): walk with an explicit `Stack`/queue inside a single builder | |
| FR0059 | A `private` function returning `Some`/`None` moves to `ValueSome`/`ValueNone` (fix): definition constructors and every match site rewritten together — no heap allocation per call; any use where `option` is load-bearing (`List.tryPick f`, `Option.*` pipelines, `let`-bound results, explicit annotations) suppresses the whole suggestion | |
| FR0060 | Consecutive attribute brackets merge: `[<Attr1>] [<Attr2>]` (stacked or same-line) → `[<Attr1; Attr2>]` (fix); comments between brackets and `[<assembly: ...>]` targets suppress it | |
| FR0061 | `invalidArg "facotr" ...` / `ArgumentException("msg", "wrongName")` — the parameter-name string must name a real parameter of the enclosing function (CA2208, note); `nameof` keeps it honest | |
| FR0062 | Non-private module-level `let mutable` is visible global mutable state (CA2211, note); private mutables and private/internal modules stay quiet | |
| FR0063 | `raise`/`failwith` inside `finally` replaces any exception already in flight (CA2219, note); raises the finally itself catches stay quiet | |
| FR0064 | Raising runtime-reserved exceptions (`OutOfMemoryException`, `StackOverflowException`, `IndexOutOfRangeException`, `NullReferenceException`, ...) misleads catchers and debuggers (CA2201, note) | |
| FR0065 | Weak cryptography (CA5350/5351, note): MD5/SHA1/DES/TripleDES/RC2 construction, plus TLS certificate-validation bypass via `ServerCertificateValidationCallback` | |
| FR0066 | SQL assembled from strings (CA2100, note): interpolation holes, `+` chains or `sprintf` flowing into `CommandText` or a `*Command` constructor — parameterize instead | |
| FR0067 | `DateTime.Parse s` / `Double.Parse s` without a culture reads differently per server culture (CA1305, note); integer parses are low-risk and stay quiet | |
| FR0068 | Duplicate enum literal values (`Red = 1 ... Crimson = 1`) silently conflate cases (CA1069, note) | |
| FR0069 | A private/internal record field `X: int option` / `DateTime option` / `Guid option` boxes the struct payload; `voption` keeps it flat — contained visibility keeps the migration contained (note) | |
| FR0070 | A private/internal record of at most four small struct fields can be `[<Struct>]`, removing a heap allocation per instance (note) | |
| FR0093 | A private/internal record field `X: int * int` is a reference tuple — one heap object per value — where `struct (int * int)` stores it inline. At most four elements, since a struct tuple is copied by value. Note only: every construction and destructuring of the field needs the `struct` keyword too | |
| FR0071 | A pure binding inside a `for`/`while`/collection lambda that depends on nothing the loop changes is re-evaluated every iteration; the fix hoists it above the loop | |
| FR0072 | A DU match wildcard standing in for only 1-2 concrete cases is an open else; the fix expands them (`_` → `D`), so future union growth raises incomplete-match warnings | |
| FR0073 | `let! x = comp` whose binder exists only to be matched collapses to `match! comp with` (F# 4.5+) | |
| FR0074 | Nested record copy-and-update flattens to F# 8 path syntax: `{ r with X = { r.X with Y = v } }` → `{ r with X.Y = v }` (LangVersion-gated; fields named after their type stay nested — the flattened path would resolve as the type) | |
| FR0075 | A locally constructed disposable bound with `let` is never disposed: fix to `use` when every mention stays in scope, advice when it escapes; manual `Dispose()` calls exempt | |
| FR0076 | `List/Array.map f \|> ignore` allocates a discarded list — fix to `iter (f >> ignore)`; `Seq.map f \|> ignore` is lazy and runs NOTHING (advice, the FR0017 family) | |
| FR0077 | An object expression missing interface members (FS0366) gets `NotImplementedException` stubs for every missing method/property, inherited interfaces in their own `interface X with` sections — the only rule that runs on non-compiling code, which is its point | |
| FR0078 | The three-part mutable-condition loop idiom (`let! first` / `let mutable go` / rebind at loop end) collapses to F# 8 `while!` — a lone stale-bool `while` never matches, `while!` re-evaluates per iteration | |
| FR0079 | `Task.WhenAll [\| t \|]` / `Task.WaitAll` / `Async.Parallel [ c ]` over a single-element literal adds indirection for nothing (CA1842/CA1843, note — the direct form changes the result type, so the author picks the landing shape) | |
| FR0080 | Leading TABs (FS1161 — pasted code often brings them) expand to four spaces per tab, every affected line in one fix; files with triple-quoted/verbatim strings are skipped (a tab could be string content) | |
| FR0081 | Path fragments joined with a hard-coded `/` or `\` separator → `Path.Combine` advice; `\` fires alone, `/` needs positive path evidence (path-ish names, rooted/extension literals, or a literal existing on disk) and URL-smelling chains never fire | |
| FR0082 | `[<FooAttribute>]` → `[<Foo>]` — the compiler resolves the short form | |
| FR0083 | `[<Foo()>]` → `[<Foo>]` — an empty attribute argument list says nothing | |
| FR0084 | ` ``name`` ` backticks around a plain non-keyword identifier do nothing; stripped per site | |
| FR0085 | `new` on a non-IDisposable construction is noise — F# convention reserves `new` for disposables (the compiler warns the inverse as FS0760); typed-gated | |
| FR0086 | `$"no holes"` → `"no holes"` — hole-free interpolation; skipped when escaped braces would need unescaping | |
| FR0087 | The pattern `x :: []` → `[ x ]` | |
| FR0088 | `Case(_, _)` → `Case _` when every field is a wildcard (typed-gated to real union cases; survives field-count changes) | |
| FR0089 | `[ 1, 2 ]` is a SINGLE-tuple list — `,` builds a tuple, `;` separates elements (note; the classic paste trap, single-tuple lists are sometimes intended) | |
| FR0090 | Tupled → curried for internal/public functions with every project call site rewritten (cross-file; `fsharp-refactor --api-changes` only, editors get the private-only FR0008) |  |
| FR0091 | Data-last parameter reorder for internal/public functions with every project call site rewritten (cross-file; `fsharp-refactor --api-changes` only, editors get the private-only FR0023). The two parameters must have different concrete types, so that a call site outside the project — which we can neither see nor fix — fails to compile rather than silently swapping two interchangeable arguments |  |
| FR0092 | A constant `failwith "Error"` gains the enclosing function's arguments — `failwith $"Error, calling mymethod with x: {x}"` — so the log says which call failed, not just which line. Static messages only; an already-interpolated one was written deliberately, as was one that already names a parameter |  |
| — | DU case payload → named record (cross-file) | needs FSAC codefix infra |

## Configuration

Rules can be disabled per repository with an optional `fsharprefactorings.json`,
searched upward from each analyzed file (the nearest one wins). Keys are rule
codes or analyzer names, case-insensitive; every rule defaults to enabled, and
a malformed file fails open so it can never break the editor. Comments and
trailing commas are tolerated:

```json
{
  "rules": {
    "FR0003": false,
    "conversionMove": { "enabled": false }
  }
}
```

A disabled rule skips its analysis entirely, so the file also works as a
performance lever on large codebases. Internally all analyzers share one
memoized AST traversal per file version, so the editor pays for a single
walk per keystroke regardless of how many rules are active.

The same file can add custom FR0012 term-rewriting rules using FSharpLint's
hint syntax (single-letter identifiers are metavariables):

```json
{
  "hints": {
    "add": [
      "Option.isSome x |> not ===> Option.isNone x"
    ]
  }
}
```

Custom rules get the same safety treatment as the built-ins: bindings are
parenthesized as needed, and a rule whose right side drops or duplicates a
metavariable only fires on pure atoms (never discarding a side effect).

---

# Improving it

Contributions welcome. This section is for working ON the analyzers; everything
above is for using them.

## Trying your changes

Build the analyzers, then point either host at the build output instead of the
NuGet cache.

In an editor, via the target repo's `.vscode/settings.json`:

```json
{
  "FSharp.enableAnalyzers": true,
  "FSharp.analyzersPath": ["<path-to>/FSharp.Refactorings.Analyzers/bin/Debug/net8.0"]
}
```

Open an F# file containing e.g. `match x with | true -> 1 | false -> 2` — a
hint appears offering `if x then 1 else 2`.

Or from the CLI:

```bash
fsharp-analyzers --project YourProject.fsproj --analyzers-path src/FSharp.Refactorings.Analyzers/bin/Debug/net8.0 --code-root .
```

Note: analyzers must be built against an FSharp.Compiler.Service compatible with
the host FsAutoComplete. This project currently pins FSharp.Analyzers.SDK 0.37.2
(FCS 43.12.201). See the SDK's version-pairing table when updating.

## Building and testing

```bash
dotnet build
dotnet test
```

This project eats its own dog food. Before committing:

```bash
dotnet tool restore
dotnet fantomas src tests
dotnet dotnet-fsharplint lint src/FSharp.Refactorings.Analyzers/FSharp.Refactorings.Analyzers.fsproj
dotnet dotnet-fsharplint lint src/FSharp.Refactorings.Apply/FSharp.Refactorings.Apply.fsproj
dotnet dotnet-fsharplint lint tests/FSharp.Refactorings.Tests/FSharp.Refactorings.Tests.fsproj
```

and the analyzers are run against their own source, expecting zero findings.
Both projects, not just the analyzers — the apply tool is F# we ship too, and
it went a long time unchecked:

```bash
dotnet tool run fsharp-analyzers --project src/FSharp.Refactorings.Analyzers/FSharp.Refactorings.Analyzers.fsproj --analyzers-path src/FSharp.Refactorings.Analyzers/bin/Debug/net8.0 --code-root .
dotnet tool run fsharp-analyzers --project src/FSharp.Refactorings.Apply/FSharp.Refactorings.Apply.fsproj --analyzers-path src/FSharp.Refactorings.Analyzers/bin/Debug/net8.0 --code-root .
```

Test inputs are string literals, so formatting tools never touch the
deliberately-shaped source fragments the tests exercise.

## Design principles

1. **Never break user code.** A fix is only offered when it is provably safe to apply;
   borderline cases simply don't produce a suggestion. Fixes are minimal range-based
   text edits applied by the editor, so they are always a single native undo step.
2. **Minimal edits.** Original formatting outside the edited range is untouched —
   no whole-file reformatting.
3. **Pure core, thin adapters.** Each refactoring is a pure function
   `ParsedInput -> ISourceText -> Suggestion list`, unit-tested directly against
   source strings. The SDK analyzer entry points in `Analyzers.fs` are one-liners.
4. **Hints point toward idiomatic F# only.** Every analyzer rewrites `a → b`
   where `b` is the more idiomatic form; we never ship a hint that moves code
   *away* from idiomatic F#. That is why suggestions are `Hint` severity, not
   warnings: they mark an opportunity, not a defect, and they never gate CI.
   Genuinely reversible rewrites where neither direction is more idiomatic
   (`if ↔ match`, tupled ↔ curried) belong in FsAutoComplete's codefix
   infrastructure as user-invoked `refactor.rewrite` actions, and should be
   contributed there rather than here.

