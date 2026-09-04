/// What kind of change each rule proposes.
///
/// The distinction that matters in practice is whether a fix is worth another
/// person's review time. Running the whole rule set over a repository you do
/// not maintain and opening a pull request from the result is a good way to
/// waste everyone's afternoon: nobody wants "removed an empty attribute
/// argument list" across two hundred files. A disposable that is never
/// disposed is a different conversation.
///
///     fsharp-refactor Their.fsproj --categories correctness,performance
///
/// is the run for someone else's codebase. Everything is the run for your own.
///
/// The four categories:
///
///   correctness — a defect. The code does something other than what it
///                 looks like it does: a race, a swallowed exception, a
///                 disposable that leaks, a comparison that never holds.
///   performance — measurably wasteful, but correct. Allocations that need
///                 not happen, repeated work, a scan where a lookup would do.
///   idiom       — the same behaviour written the way F# writes it. Worth
///                 doing, and worth agreeing on first: it is a matter of
///                 house style as much as anything.
///   cosmetic    — punctuation and spelling of code. Real cleanups, and
///                 nobody's idea of a welcome pull request from a stranger.
module FSharp.Refactor.RuleCatalog

open System

[<RequireQualifiedAccess>]
type Category =
    | Correctness
    | Performance
    | Idiom
    | Cosmetic

let name (category: Category) =
    match category with
    | Category.Correctness -> "correctness"
    | Category.Performance -> "performance"
    | Category.Idiom -> "idiom"
    | Category.Cosmetic -> "cosmetic"

let all =
    [ Category.Correctness
      Category.Performance
      Category.Idiom
      Category.Cosmetic ]

let parse (text: string) =
    let wanted = text.Trim()

    all
    |> List.tryFind (fun c -> String.Equals(name c, wanted, StringComparison.OrdinalIgnoreCase))

/// The categories a stranger's repository is worth a pull request over.
let substantive = set [ Category.Correctness; Category.Performance ]

/// Every rule, by code. A rule absent here reads as `idiom`, which keeps a
/// newly added rule out of `--categories correctness` until someone has
/// decided where it belongs.
let private categories =
    [ // --- correctness: the code does not do what it looks like it does
      "FR0017", Category.Correctness // Async discarded with ignore never runs
      "FR0018", Category.Correctness // check-then-add races
      "FR0019", Category.Correctness // Equals without GetHashCode
      "FR0020", Category.Correctness // abstract member called during construction
      "FR0027", Category.Correctness // lambda capturing this holds the object alive
      "FR0032", Category.Correctness // disposable field, no IDisposable
      "FR0036", Category.Correctness // runtime type comparison breaks on a rename
      "FR0044", Category.Correctness // raise ex resets the stack trace
      "FR0045", Category.Correctness // x = nan never holds
      "FR0046", Category.Correctness // locking a process-wide singleton
      "FR0047", Category.Correctness // Dispose that misses a field
      "FR0048", Category.Correctness // String.Format placeholder without an argument
      "FR0049", Category.Correctness // sync-over-async deadlocks
      "FR0054", Category.Correctness // raise inside Equals/GetHashCode/Dispose
      "FR0055", Category.Correctness // swallowing every exception
      "FR0061", Category.Correctness // invalidArg naming a parameter that does not exist
      "FR0062", Category.Correctness // public module-level mutable state
      "FR0063", Category.Correctness // raise in finally discards the exception in flight
      "FR0064", Category.Correctness // raising runtime-reserved exceptions
      "FR0065", Category.Correctness // weak cryptography
      "FR0066", Category.Correctness // SQL assembled from strings
      "FR0067", Category.Correctness // culture-sensitive parsing
      "FR0068", Category.Correctness // duplicate enum values conflate cases
      "FR0072", Category.Correctness // a wildcard hiding one or two real cases
      "FR0075", Category.Correctness // a disposable bound with let is never disposed
      "FR0077", Category.Correctness // object expression missing interface members (FS0366)
      "FR0080", Category.Correctness // leading TABs (FS1161)
      "FR0089", Category.Correctness // [ 1, 2 ] is a one-element list of a tuple
      "FR0100", Category.Correctness // an unfinished branch returning a plausible value
      "FR0105", Category.Correctness // unchecked arithmetic on near-limit constants wraps silently
      "FR0110", Category.Correctness // incomplete DU match gains explicit raising arms (FS0025 made loud)

      // --- performance: correct, but doing work it need not
      "FR0004", Category.Performance
      "FR0014", Category.Performance // ContainsKey + indexer: two lookups (measured 1.26x); the ConcurrentDictionary race is called out in the message
      "FR0011", Category.Performance
      "FR0015", Category.Performance
      "FR0016", Category.Performance
      "FR0021", Category.Performance
      "FR0028", Category.Performance // N+1 queries
      "FR0029", Category.Performance // task state machine
      "FR0030", Category.Performance
      "FR0035", Category.Performance
      "FR0037", Category.Performance
      "FR0038", Category.Performance
      "FR0039", Category.Performance
      "FR0040", Category.Performance
      "FR0041", Category.Performance
      "FR0051", Category.Performance
      "FR0052", Category.Performance
      "FR0053", Category.Performance
      "FR0058", Category.Performance
      "FR0059", Category.Performance
      "FR0069", Category.Performance
      "FR0070", Category.Performance
      "FR0071", Category.Performance
      "FR0076", Category.Performance
      "FR0079", Category.Performance
      "FR0093", Category.Performance
      "FR0102", Category.Performance // list indexing in a loop is O(i) per access
      "FR0106", Category.Performance // Substring copy fed to a parser; AsSpan is 2.6x and allocation-free
      "FR0104", Category.Performance // singleton append per recursive call is O(n²)

      // --- idiom: same behaviour, written the way F# writes it
      "FR0001", Category.Idiom
      "FR0002", Category.Idiom
      "FR0003", Category.Idiom
      "FR0005", Category.Idiom
      "FR0006", Category.Idiom
      "FR0007", Category.Idiom
      "FR0008", Category.Idiom
      "FR0009", Category.Idiom
      "FR0010", Category.Idiom
      "FR0012", Category.Idiom
      "FR0022", Category.Idiom
      "FR0023", Category.Idiom
      "FR0024", Category.Idiom
      "FR0025", Category.Idiom
      "FR0026", Category.Idiom
      "FR0031", Category.Idiom
      "FR0033", Category.Idiom
      "FR0034", Category.Idiom
      "FR0042", Category.Idiom
      "FR0043", Category.Idiom
      "FR0050", Category.Idiom // measured: List/Array.sum run LEVEL with the loop, not faster
      "FR0107", Category.Idiom // exists/forall short-circuit where the flag loop kept iterating
      "FR0108", Category.Idiom // && true contributes nothing; the expression is the other operand
      "FR0109", Category.Idiom // a || a is a, when a is visibly call-free; often a copy-paste worth a look
      "FR0112", Category.Idiom // if/elif over one ident vs literals is a match spelled longhand
      "FR0113", Category.Idiom // nested ifs with the same else (or none) merge into one &&
      "FR0114", Category.Idiom // pyramid flip: short exit first (default off - house style)
      "FR0115", Category.Idiom // base case first behind a compound guard: advice on arm order
      "FR0116", Category.Idiom // a non-recursive member leaves its let rec group
      "FR0117", Category.Idiom // adjacent same-result match arms fold into an or-pattern
      "FR0118", Category.Correctness // a CancellationToken in scope should reach the calls that take one
      "FR0119", Category.Correctness // inside task/async, the awaitable twin should be used
      "FR0120", Category.Correctness // a catch-clause log should mention the caught exception
      "FR0121", Category.Correctness // UtcNow.Date/Today are timezone-random; Now->UtcNow opt-in
      "FR0122", Category.Correctness // literal regex patterns must compile
      "FR0123", Category.Correctness // Monitor.Enter/Exit is the lock function spelled dangerously
      "FR0124", Category.Correctness // structured-log templates that lie
      "FR0125", Category.Correctness // invisible and bidirectional Unicode in source
      "FR0126", Category.Correctness // dynamic strings into process-execution sinks
      "FR0127", Category.Correctness // provider-format API keys in literals
      "FR0128", Category.Idiom // obsolete crypto constructors become static factories
      "FR0129", Category.Idiom // a guard that only equality-tests the binder is the literal pattern
      "FR0130", Category.Idiom // module-level constants gain Literal
      "FR0131", Category.Idiom // provably tail-recursive functions gain TailCall
      "FR0132", Category.Idiom // trailing comment promoted to XML doc position
      "FR0133", Category.Cosmetic // five-word names become double-backtick names (tests by default)
      "FR0134", Category.Idiom // DateTime record fields migrate to DateTimeOffset (default off)
      "FR0135", Category.Cosmetic // markdown-bearing block comments in scripts become literate cells
      "FR0136", Category.Correctness // zero-argument Guid constructor: Guid.Empty stated, NewGuid offered
      "FR0137", Category.Performance // consecutive same-module map passes fuse via composition
      "FR0138", Category.Idiom // hand-rolled emptiness tests become String.IsNullOrEmpty/IsNullOrWhiteSpace
      "FR0139", Category.Performance // Seq.* on a proven array goes through IEnumerable
      "FR0140", Category.Idiom // constructor then property sets is named-property construction spelled out
      "FR0141", Category.Idiom // a state-carrying while loop is tail recursion written inside out
      "FR0142", Category.Performance // a test that blocks on async work returns the work as a Task instead
      "FR0143", Category.Correctness // a script #load chain missing a file of the project it loads from
      "FR0144", Category.Correctness // a script #r or #I path the package no longer has, re-pointed at what it has
      "FR0145", Category.Correctness // a record expression leaving fields unassigned gets them, by type
      "FR0073", Category.Idiom
      "FR0074", Category.Idiom
      "FR0078", Category.Idiom
      "FR0081", Category.Idiom
      "FR0087", Category.Idiom
      "FR0090", Category.Idiom
      "FR0091", Category.Idiom
      "FR0092", Category.Idiom
      "FR0095", Category.Idiom
      "FR0101", Category.Idiom // index-based loop over a collection it only indexes
      "FR0103", Category.Idiom // isinstance-style type-test ladders as match

      // --- cosmetic: punctuation and spelling of code
      "FR0013", Category.Cosmetic
      "FR0057", Category.Cosmetic // XML doc drift
      "FR0060", Category.Cosmetic
      "FR0082", Category.Cosmetic
      "FR0083", Category.Cosmetic
      "FR0084", Category.Cosmetic
      "FR0085", Category.Cosmetic
      "FR0086", Category.Cosmetic
      "FR0088", Category.Cosmetic
      "FR0094", Category.Cosmetic
      "FR0096", Category.Cosmetic
      "FR0097", Category.Cosmetic
      "FR0098", Category.Cosmetic
      "FR0111", Category.Cosmetic // else-holding-an-if is elif spelled tall
      "FR0099", Category.Cosmetic ]
    |> Map.ofList

/// The category of a rule; anything unlisted reads as `idiom`.
let categoryOf (code: string) =
    categories.TryFind(code.ToUpperInvariant())
    |> Option.defaultValue Category.Idiom

/// Every known code in the given categories.
let codesIn (wanted: Set<Category>) =
    categories
    |> Map.toSeq
    |> Seq.filter (fun (_, category) -> wanted.Contains category)
    |> Seq.map fst
    |> Set.ofSeq

/// Every code the catalog knows, for tests that check nothing was forgotten.
let known = categories |> Map.toSeq |> Seq.map fst |> Set.ofSeq

/// Every rule as (code, category) — the machine-readable catalog.
let allRules = categories |> Map.toList
