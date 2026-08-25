# FSharp.Refactorings

ReSharper-style **functional refactoring suggestions for F#**, delivered as light-bulb
quick fixes. Built on [FSharp.Analyzers.SDK](https://github.com/ionide/FSharp.Analyzers.SDK),
so the same analyzers run in:

- **VS Code / Ionide** (via FsAutoComplete) — diagnostics with one-click fixes
- **CLI / CI** (via the [`fsharp-analyzers`](https://www.nuget.org/packages/fsharp-analyzers) dotnet tool)

Visual Studio 2022 does not load F# analyzers; the plan for VS is to upstream the
most valuable fixes into `dotnet/fsharp`'s `FSharp.Editor` code fixes.

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
| FR0034 | `if x.IsSome then x.Value + 1 else e` → `match x with \| Some v -> v + 1 \| None -> e` (`.Value` throws when misused; the match cannot); handles the `IsNone`/negated forms, else-less unit `if`, `x.Value.P` prefixes, and spells `ValueSome`/`ValueNone` when the receiver is a voption (typed-gated, so custom `IsSome`/`Value` members never match) | |
| FR0035 | `List/Array/Seq.contains x ys` inside a loop — or inside a callback given to a collection function — scans `ys` linearly per iteration; note suggests building a Set once outside (probing the loop variable itself never fires) | |
| FR0036 | Fragile runtime type comparisons (notes): `GetType().Name = "..."` breaks silently on renames/namespaces — compare types instead; `x.GetType() = typeof<T>` is exact-type equality — `x :? T` if subtypes are fine | |
| FR0037 | Build-once types constructed inside a loop: `ConcurrentDictionary`, `JsonSerializerOptions` (CA1869), `SearchValues.Create` (CA1870) — all expensive by design; note suggests hoisting out or making static | |
| FR0038 | Char overloads for single-character strings (CA1834/1847/1865-67): `s.Contains "x"` → `s.Contains 'x'` and `sb.Append "x"` → `sb.Append 'x'` (both ordinal already — fix); `s.StartsWith("x", StringComparison.Ordinal)` → `s.StartsWith('x')` (fix); bare `StartsWith`/`EndsWith`/`IndexOf` are culture-sensitive where the char overload is ordinal, so those get an advisory note only; receivers typed-gated to `String`/`StringBuilder` | |
| FR0039 | Allocating case-insensitive comparisons (CA1862, note): `a.ToLower() = b.ToLower()` and `s.ToLower().StartsWith "abc"` allocate lowered copies just to compare; `String.Equals(a, b, StringComparison...IgnoreCase)` / the comparison overloads are allocation-free — comparison type stays the author's deliberate choice | |
| FR0040 | Redundant membership guards (CA1853/1868, fix): `if d.ContainsKey k then d.Remove k \|> ignore` → `d.Remove k \|> ignore`, `if not (s.Contains x) then s.Add x \|> ignore` → `s.Add x \|> ignore` — the operations already return `false` on a miss; typed-gated to `Dictionary`/`HashSet`/`SortedSet` | |
| FR0041 | `Array.sum/average/min/max` on `int[]`/`int64[]` is a scalar loop; on .NET 8+ System.Linq's `Sum()`/`Average()`/`Min()`/`Max()` are SIMD-vectorized (note only: LINQ `Sum` throws on overflow where `Array.sum` wraps; floats excluded — NaN semantics differ) | |
| FR0042 | Fully applied `sprintf` → typed interpolated string (`sprintf "asdf %s" x` → `$"asdf %s{x}"`); specifiers are kept verbatim so the output is byte-identical; guards: regular literal with no `{`/`}`, simple arguments only, no `%a`/`%t`/`*`-widths, partial applications never match | |
| FR0043 | In an interpolated string that *already* has a typed hole, the remaining plain holes gain specifiers (`$"%s{name} is {age}"` → `$"%s{name} is %d{age}"`) — free compile-time type pinning since the string is on the printf path anyway; specifier-free strings are left on the F# 8 `String.Concat` fast path, and only ToString-identical specifiers are used (`%s`/`%d`/`%c`; never `%b` or `%f`) | |
| — | Tupled → curried for public functions (cross-file) | needs FSAC codefix infra |
| — | Reorder parameters (cross-file) | needs FSAC codefix infra |
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
```

and the analyzers are run against their own source (expecting zero findings):

```bash
dotnet tool run fsharp-analyzers --project src/FSharp.Refactorings.Analyzers/FSharp.Refactorings.Analyzers.fsproj --analyzers-path src/FSharp.Refactorings.Analyzers/bin/Debug/net8.0 --code-root .
```

Test inputs are string literals, so formatting tools never touch the
deliberately-shaped source fragments the tests exercise.

## Trying it in VS Code (Ionide)

1. `dotnet build src/FSharp.Refactorings.Analyzers`
2. In the target repo's `.vscode/settings.json`:

   ```json
   {
     "FSharp.enableAnalyzers": true,
     "FSharp.analyzersPath": ["<path-to>/FSharp.Refactorings.Analyzers/bin/Debug/net8.0"]
   }
   ```

3. Open an F# file containing e.g. `match x with | true -> 1 | false -> 2` —
   a hint appears with a quick fix to rewrite it as `if x then 1 else 2`.

Note: analyzers must be built against an FSharp.Compiler.Service compatible with
the host FsAutoComplete. This project currently pins FSharp.Analyzers.SDK 0.37.2
(FCS 43.12.201). See the SDK's version-pairing table when updating.

## Running from the CLI

```bash
dotnet tool install --global fsharp-analyzers
fsharp-analyzers --project YourProject.fsproj --analyzers-path src/FSharp.Refactorings.Analyzers/bin/Debug/net8.0 --code-root .
```
