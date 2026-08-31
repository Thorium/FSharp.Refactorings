/// Applies the FSharp.Refactor analyzers' quick fixes from the command line.
///
///     dotnet tool install --global fsharp-refactor
///     fsharp-refactor Your.fsproj
///
/// (from this repository:
/// `dotnet run --project src/FSharp.Refactor.Tool -- Your.fsproj`)
///
/// The stock `fsharp-analyzers` CLI only REPORTS: fixes reach editors and
/// SARIF, never the files. This tool closes that gap:
///
///   1. `dotnet msbuild --getItem:FscCommandLineArgs` yields the exact
///      compiler arguments (no project-cracking library needed)
///   2. every [<CliAnalyzer>] in FSharp.Refactor.Analyzers runs against
///      each source file via reflection — new rules are picked up
///      automatically
///   3. non-overlapping fixes are applied bottom-up per file; because a fix
///      can enable further fixes (or invalidate siblings), analysis re-runs
///      until a pass applies nothing (bounded by --max-passes)
///   4. a final re-check compares the project's error count against the
///      baseline and fails loudly if applying introduced any
///
/// The first bare argument says what to fix, and its kind is read off the
/// path — no flag needed:
///
///     Your.fsproj      one project
///     Thing.fs         one source file: its project is found and analyzed,
///                      but only that file is edited
///     build.fsx        one script; needs no MSBuild at all, so it starts
///                      analysing immediately, and #load'ed files come along
///     Your.sln/.slnx   every F# project the solution lists
///     src/             the solution in that directory, or the projects under it
///     "src/**/*.fsproj"  everything the glob matches
///
/// Options:
///     --project / --script      accepted as aliases; the kind is inferred
///                               from the extension either way
///     --codes FR0002,FR0031    only apply these rule codes
///     --categories correctness only apply rules of these kinds
///     --dry-run                report what would be applied, change nothing
///     --api-changes            also apply CROSS-FILE fixes (rules that
///                              rewrite call sites of internal symbols
///                              across the project); without it those
///                              fixes are held back and counted. Public
///                              symbols are never rewritten: their callers
///                              can live outside the checked project
///     --no-if-defs             never emit #if/#else/#endif pairs for
///                              capability fixes on multi-targeted
///                              projects; fixes stay plain and the final
///                              build check alone decides their fate
///     --report <file>          write every surfaced finding as SARIF
///                              2.1.0 for CI annotation
///     --jobs <n>               files typechecked at once (default 4, capped
///                              by the core count). Trades CPU for wall
///                              clock: FCS reuses each file's prefix within
///                              one incremental build, and parallel checks
///                              give that reuse up, so the gain peaks around
///                              4 and reverses if pushed higher. --jobs 1
///                              restores the sequential sweep
///     --framework <tfm>        analyze against just this target framework.
///                              By default a multi-targeted project is worked
///                              through framework by framework, narrowest
///                              first: each activates its own #if branches,
///                              and code behind another one's is not in the
///                              parse tree at all. Every pass ends by building
///                              all the frameworks, so a fix that suits one
///                              but not the others fails loudly
///     --max-passes <n>         fix-then-reanalyze iterations (default 5)
module FSharp.Refactor.Tool.Program

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text.Json
open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FSharp.Refactor
open FSharp.Refactor.Text

type private Options =
    {
        Target: string
        ShowHelp: bool
        Codes: Set<string> option
        /// The codes the user TYPED in --codes, before any --categories
        /// expansion is folded into `Codes`. Only these outrank a rule's
        /// default-off status or a config disable: naming a rule is an
        /// ask, a category is merely a filter.
        ExplicitCodes: Set<string> option
        /// Narrows `Codes` once parsing is done, so the two flags combine the
        /// same way whichever order they were given in.
        Categories: Set<RuleCatalog.Category> option
        DryRun: bool
        ApiChanges: bool
        /// Suppresses dual-framework #if pair emission: capability fixes
        /// stay plain everywhere, and the all-frameworks build check alone
        /// decides whether they survive on a legacy-targeting project.
        NoIfDefs: bool
        /// SARIF 2.1.0 output file: every finding the run surfaced, for CI
        /// annotation. Pairs naturally with --dry-run.
        Report: string option
        /// No MSBuild, no reference resolution: sources are read straight
        /// from the fsproj and only the analyzers that never consult the
        /// typechecker run. For codebases that cannot compile on this
        /// machine (a type provider needing its database, say).
        ParseOnly: bool
        /// A previous run's SARIF: findings whose fingerprints appear in it
        /// are neither reported nor fixed — the ratchet for agents and CI.
        Baseline: string option
        /// Exit 3 when the run surfaced any finding (after baseline
        /// filtering) — the hard lint gate.
        FailOnFindings: bool
        /// Honor every suppression comment regardless of the config's
        /// "suppressions" policy — the CI override.
        HonorSuppressions: bool
        /// List fix-less advisory notes inline instead of the one-line
        /// per-category summary.
        Notes: bool
        /// Machine-readable stdout: prose moves to stderr, and the run's
        /// findings leave as one JSON document on stdout.
        Json: bool
        /// Print the rule catalog and exit.
        ListRules: bool
        /// Serve analyze/list_rules over MCP (JSON-RPC on stdio).
        Mcp: bool
        MaxPasses: int
        Jobs: int
        /// Overrides the automatic narrowest-framework choice, so the code
        /// behind another framework's #if can be reached.
        Framework: string
    }

let private helpText =
    """fsharp-refactor — applies F# refactoring quick fixes to your code.

USAGE
  fsharp-refactor <what> [options]

WHAT TO FIX — the kind is read off the path, no flag needed:
  Your.fsproj           one project
  Thing.fs              one source file; its project is found and analysed,
                        but only that file is edited
  build.fsx             one script; no MSBuild step at all, so it starts at once
  Your.sln, Your.slnx   every F# project the solution lists
  src/                  the solution in that directory, or the projects beneath
  "src/**/*.fsproj"     everything the glob matches

OPTIONS
  --dry-run             report every fix, change nothing. Rewriting is never
                        implicit: without this it edits, with it it does not
  --codes FR0002,FR0031 only these rules
  --categories <list>   only rules of these kinds: correctness, performance,
                        idiom, cosmetic. Combined with --codes it narrows
                        further. For a repository you do not maintain,
                        "correctness,performance" is the set worth a pull
                        request; nobody welcomes a stranger's punctuation
  --jobs <n>            files typechecked at once (default 4, clamped to 2-4 by
                        core count). --jobs 1 is the sequential sweep
  --framework <tfm>     analyse only this target framework. By default a
                        multi-targeted project is worked through framework by
                        framework, narrowest first, because code behind another
                        framework's #if is not in the parse tree at all
  --api-changes         also apply cross-file fixes that change internal
                        signatures, rewriting call sites project-wide. Held
                        back and merely counted without this. Public
                        signatures are never rewritten: their callers can
                        live outside the checked project
  --no-if-defs          never emit #if/#else/#endif pairs for capability
                        fixes on multi-targeted projects. The fixes stay
                        plain, and any that break a legacy framework are
                        simply put back by the final build check
  --report <file>       write every finding as SARIF 2.1.0 — the format CI
                        turns into inline annotations. Pairs with --dry-run
  --parse-only          no MSBuild, no reference resolution: sources come
                        straight from the fsproj and only the syntactic
                        analyzers run. For codebases that cannot compile on
                        this machine (a type provider needing its database);
                        typed rules and #if branches are out of reach
  --baseline <sarif>    findings whose fingerprints appear in this earlier
                        report are neither reported nor fixed: the ratchet.
                        Triage once, then only NEW findings surface
  --fail-on-findings    exit 3 when any finding survives the filters — the
                        hard CI gate (0 clean, 1 failure, 2 usage)
  --honor-suppressions  honor every suppression comment regardless of the
                        config's "suppressions" policy — the CI override
                        for a repo that wants comments inert locally
  --notes               list fix-less advisory notes inline. By default a
                        run prints its FIXES and ends with one per-category
                        note count; SARIF (--report) and --format json
                        always carry the notes in full
  --format json         machine-readable stdout: progress prose moves to
                        stderr and the findings leave as one JSON document.
                        The default stays human-readable
  --rules               print the rule catalog (honors --format json)
  --mcp                 serve analyze/list_rules as an MCP server over
                        stdio, keeping the typechecker warm between calls
  --max-passes <n>      fix-then-reanalyse iterations (default 5)
  --help, -h, /?        this text

A run refuses a compilation that already has errors, and fails loudly if
applying introduces one. For a multi-targeted project every framework is built
before it reports success.

Rules can be turned off per repository with a fsharprefactor.json.
Full documentation: https://github.com/Thorium/fsharp-refactor"""

[<TailCall>]
let rec private parseArgsLoop opts args =
    match args with
    | [] -> Ok opts
    // --project and --script still work, but the kind is inferred from the
    // extension either way, so a bare path is enough
    | "--project" :: path :: rest
    | "--script" :: path :: rest -> parseArgsLoop { opts with Target = path } rest
    | "--codes" :: codes :: rest ->
        let parsed = codes.Split(',') |> Array.map _.Trim() |> Set.ofArray

        parseArgsLoop
            { opts with
                Codes = Some parsed
                ExplicitCodes = Some parsed }
            rest
    | "--categories" :: names :: rest ->
        let parsed = names.Split(',') |> Array.map RuleCatalog.parse

        match parsed |> Array.tryFindIndex Option.isNone with
        | Some bad ->
            let known = RuleCatalog.all |> List.map RuleCatalog.name |> String.concat ", "
            Error $"'{names.Split(',').[bad].Trim()}' is not a category. Known categories: {known}."
        | None ->
            parseArgsLoop
                { opts with
                    Categories = Some(parsed |> Array.choose id |> Set.ofArray) }
                rest
    | "--help" :: _
    | "-h" :: _
    | "/?" :: _
    | "-?" :: _ -> Ok { opts with ShowHelp = true }
    | "--dry-run" :: rest -> parseArgsLoop { opts with DryRun = true } rest
    | "--api-changes" :: rest -> parseArgsLoop { opts with ApiChanges = true } rest
    | "--no-if-defs" :: rest -> parseArgsLoop { opts with NoIfDefs = true } rest
    | "--report" :: file :: rest -> parseArgsLoop { opts with Report = Some file } rest
    | "--parse-only" :: rest -> parseArgsLoop { opts with ParseOnly = true } rest
    | "--baseline" :: file :: rest -> parseArgsLoop { opts with Baseline = Some file } rest
    | "--fail-on-findings" :: rest -> parseArgsLoop { opts with FailOnFindings = true } rest
    | "--honor-suppressions" :: rest -> parseArgsLoop { opts with HonorSuppressions = true } rest
    | "--notes" :: rest -> parseArgsLoop { opts with Notes = true } rest
    | "--format" :: "json" :: rest -> parseArgsLoop { opts with Json = true } rest
    | "--format" :: other :: _ -> Error $"--format knows 'json' (the default output is human-readable); got '{other}'"
    | "--rules" :: rest -> parseArgsLoop { opts with ListRules = true } rest
    | "--mcp" :: rest -> parseArgsLoop { opts with Mcp = true } rest
    | "--framework" :: tfm :: rest -> parseArgsLoop { opts with Framework = tfm } rest
    | "--jobs" :: n :: rest ->
        match Int32.TryParse n with
        | true, jobs when jobs > 0 -> parseArgsLoop { opts with Jobs = jobs } rest
        | _ -> Error $"--jobs needs a positive number, got '{n}'"
    | "--max-passes" :: n :: rest ->
        match Int32.TryParse n with
        | true, passes when passes > 0 -> parseArgsLoop { opts with MaxPasses = passes } rest
        | _ -> Error $"--max-passes needs a positive number, got '{n}'"
    // a bare path: the common case, no flag needed
    | path :: rest when not (path.StartsWith '-') && opts.Target = "" -> parseArgsLoop { opts with Target = path } rest
    | unknown :: _ -> Error $"Unknown argument '{unknown}'"

/// Fold `--categories` into `--codes`. Doing it here rather than in the loop
/// keeps the two flags order-independent, and leaves one code filter for the
/// rest of the tool to consult.
let private applyCategories (opts: Options) =
    match opts.Categories with
    | None -> opts
    | Some wanted ->
        let fromCategories = RuleCatalog.codesIn wanted

        let combined =
            opts.Codes
            |> Option.map (fun explicitCodes -> Set.intersect explicitCodes fromCategories)
            |> Option.defaultValue fromCategories

        { opts with Codes = Some combined }

let private parseArgs (argv: string[]) =
    parseArgsLoop
        { Target = ""
          ShowHelp = false
          Codes = None
          ExplicitCodes = None
          Categories = None
          DryRun = false
          ApiChanges = false
          NoIfDefs = false
          Report = None
          ParseOnly = false
          Baseline = None
          FailOnFindings = false
          HonorSuppressions = false
          Notes = false
          Json = false
          ListRules = false
          Mcp = false
          MaxPasses = 5
          // Measured sweet spot. FCS reuses each file's prefix within one
          // incremental build, so parallel checks buy wall clock by giving
          // that reuse up: on a 113-file project the sweep runs 70 s at one
          // job, 53 s at four, and back up to 61 s at eleven. Clamped to
          // 2..4 — a small machine is not oversubscribed, and even a
          // single-core one still overlaps a check with an analyzer pass.
          Jobs = min 4 (max 2 Environment.ProcessorCount)
          Framework = "" }
        (List.ofArray argv)
    |> Result.map applyCategories

/// No child process gets to hang the tool.
///
/// Three ways that happens, and all three have. Draining one pipe to
/// completion before the other deadlocks as soon as the child fills the one
/// nobody is reading. An inherited stdin lets a child that decides to prompt
/// — NuGet asking for feed credentials is the usual one — wait forever on a
/// console that may not even be attached. And a child that simply never
/// finishes takes us with it, silently, which is the worst of the three
/// because there is nothing on screen to explain it.
let private runProcess (timeout: TimeSpan) (fileName: string) (arguments: string) =
    let psi =
        ProcessStartInfo(
            FileName = fileName,
            Arguments = arguments,
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false
        )

    use p = Process.Start psi

    // a prompt now reads end-of-input and gives up, instead of waiting
    p.StandardInput.Close()

    // Both pipes drain on their own callbacks. .NET raises each stream's
    // event in order, so a builder is never written from two threads at
    // once, and nothing here blocks on a task the child has to finish
    // first — which is what made reading one pipe then the other deadlock.
    let outText = Text.StringBuilder()
    let errText = Text.StringBuilder()

    p.OutputDataReceived.Add(fun e ->
        if not (isNull e.Data) then
            outText.AppendLine e.Data |> ignore)

    p.ErrorDataReceived.Add(fun e ->
        if not (isNull e.Data) then
            errText.AppendLine e.Data |> ignore)

    p.BeginOutputReadLine()
    p.BeginErrorReadLine()

    if p.WaitForExit(int timeout.TotalMilliseconds) then
        // the timed overload can return before the output callbacks have
        // flushed; the argument-less one waits for them
        p.WaitForExit()
        p.ExitCode, outText.ToString(), errText.ToString()
    else
        try
            // the whole tree: MSBuild leaves worker nodes behind
            p.Kill true
        with
        | :? InvalidOperationException
        | :? NotSupportedException
        | :? System.ComponentModel.Win32Exception -> ()

        let minutes = timeout.TotalMinutes

        -1, "", $"'{fileName} {arguments}' had not finished after {minutes} minutes, so it was stopped."

/// Long enough for a real build of a large project, short enough that a
/// stuck one is reported rather than waited on forever.
let private processTimeout = TimeSpan.FromMinutes 15.0

/// Visual Studio's MSBuild.exe, for old-style (non-SDK) projects whose
/// imports only evaluate under it. Located via vswhere.
let private vsMsBuildPath =
    lazy
        (try
            let vswhere =
                Path.Combine(
                    Environment.GetFolderPath Environment.SpecialFolder.ProgramFilesX86,
                    "Microsoft Visual Studio",
                    "Installer",
                    "vswhere.exe"
                )

            if File.Exists vswhere then
                let _, out, _ =
                    runProcess
                        (TimeSpan.FromMinutes 1.0)
                        vswhere
                        "-latest -requires Microsoft.Component.MSBuild -find MSBuild\\**\\Bin\\MSBuild.exe"

                out.Split('\n') |> Array.map _.Trim() |> Array.tryFind File.Exists
            else
                None
         // no Visual Studio here, or it refused to answer: old-style
         // projects then fall back to the SDK's msbuild
         with
         | :? System.ComponentModel.Win32Exception
         | :? InvalidOperationException
         | :? IOException
         | :? UnauthorizedAccessException -> None)

/// How narrow a target framework's API surface is; lower sorts first.
///
/// netstandard is the smallest common denominator, then .NET Framework,
/// then .NET Core, then modern .NET by version. Within a family the lower
/// version is narrower, so netstandard2.0 sorts ahead of netstandard2.1.
/// Framework monikers have no dot (net48, net481) where modern ones do
/// (net8.0, net10.0), which is what separates the two.
let private tfmRank (tfm: string) =
    // the OS suffix's digits would swamp the version: net8.0-windows10.0.19041.0
    // must rank as net8.0, not overflow Int32 and sort before net6.0
    let t = (tfm.ToLowerInvariant().Split '-').[0]
    let digits = String(t |> Seq.filter Char.IsDigit |> Seq.toArray)

    let version =
        match Int32.TryParse digits with
        | true, v -> v
        | false, _ -> 0

    if t.StartsWith "netstandard" then 0, version
    elif t.StartsWith "netcoreapp" then 2, version
    elif t.StartsWith "net" && t.Contains '.' then 3, version
    else 1, version

/// The project's fsc arguments, straight from MSBuild. SDK-style projects
/// go through `dotnet`; old-style (net48-era) projects need Visual
/// Studio's MSBuild, whose imports do not evaluate under the SDK's.
/// --parse-only's argument source: the fsproj text itself, no MSBuild.
/// Compile items in order plus whatever DefineConstants can be read
/// literally — conditions and $() are beyond a textual read and are
/// simply skipped, so `#if` branches behind them stay out of the parse
/// (documented limitation of the mode).
// compiled once: these run per project (and the whitespace collapser per
// FINDING), and FR0015 rightly flagged the re-parsed patterns — dogfood
let private propertyGroupRegex =
    Text.RegularExpressions.Regex(
        "<PropertyGroup([^>]*)>((?s:.*?))</PropertyGroup>",
        Text.RegularExpressions.RegexOptions.Compiled
    )

let private conditionAttributeRegex =
    Text.RegularExpressions.Regex("Condition\\s*=\\s*\"([^\"]*)\"", Text.RegularExpressions.RegexOptions.Compiled)

let private defineElementRegex =
    Text.RegularExpressions.Regex(
        "<DefineConstants([^>]*)>([^<]*)</DefineConstants>",
        Text.RegularExpressions.RegexOptions.Compiled
    )

let private whitespaceRunRegex =
    Text.RegularExpressions.Regex(@"\s+", Text.RegularExpressions.RegexOptions.Compiled)

let private compileItemRegex =
    Text.RegularExpressions.Regex("<Compile\\s+Include=\"([^\"]+)\"", Text.RegularExpressions.RegexOptions.Compiled)

let private defineConstantsElementRegex =
    Text.RegularExpressions.Regex(
        "<DefineConstants[^>]*>([^<]*)</DefineConstants>",
        Text.RegularExpressions.RegexOptions.Compiled
    )

let private parseOnlyArgs (projectPath: string) =
    let projectText =
        try
            File.ReadAllText projectPath
        with
        | :? IOException
        | :? UnauthorizedAccessException -> ""

    let projectDir = Path.GetDirectoryName(Path.GetFullPath projectPath)

    // <Compile Include> is read verbatim, so an item carrying an
    // unexpanded MSBuild property ($(SourcesRoot)\X.fs) or pointing at a
    // file that is not on disk yet (mid-development trees, generated
    // sources) must be SKIPPED, not crash the sweep files later
    let sources, dropped =
        [| for m in compileItemRegex.Matches projectText -> m.Groups.[1].Value |]
        |> Array.filter (fun s -> not (s.Contains '*'))
        |> Array.partition (fun s ->
            not (s.Contains "$(")
            && (try
                    File.Exists(Path.Combine(projectDir, s.Replace('\\', Path.DirectorySeparatorChar)))
                with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                    false))

    if dropped.Length > 0 then
        eprintfn $"  ({dropped.Length} <Compile Include> item(s) skipped: unexpanded MSBuild properties or files not on disk)"

    if sources.Length = 0 then
        Error "no <Compile Include> items to parse (wildcards and imported items are beyond --parse-only)"
    else
        let defines =
            [| for m in defineConstantsElementRegex.Matches projectText do
                   for piece in m.Groups.[1].Value.Split ';' do
                       let piece = piece.Trim()

                       if piece <> "" && not (piece.Contains "$(") then
                           $"--define:{piece}" |]
            |> Array.distinct

        Ok(Array.append defines sources)

let private fscArgs (chosenFramework: string) (projectPath: string) =
    let projectText =
        try
            File.ReadAllText projectPath
        with
        | :? IOException
        | :? UnauthorizedAccessException -> ""

    let isSdkStyle = projectText.Contains "Sdk="

    // an .fsproj is not necessarily an F# compilation: SQL database
    // projects (MSBuild.Sdk.SqlProj, OutputType Database, dacpac output)
    // wear the extension too. Recognize them BEFORE paying for a
    // design-time build that can only end in "no FscCommandLineArgs"
    let isDatabaseProject =
        projectText.Contains "MSBuild.Sdk.SqlProj"
        || Text.RegularExpressions.Regex.IsMatch(
            projectText,
            "<OutputType>\\s*Database\\s*</OutputType>",
            Text.RegularExpressions.RegexOptions.IgnoreCase
        )

    if isDatabaseProject then
        Error "a SQL database project (dacpac), not an F# compilation — skipped"
    else

        // A multi-targeted project builds "outer" and dispatches one inner build
        // per framework. CoreCompile — and so FscCommandLineArgs — only runs in
        // the inner ones, so the outer query comes back empty. Pin a framework
        // to get an inner build.
        //
        // The MOST RESTRICTIVE one, not the first listed. Which matters: a rule
        // gated on what the target can resolve offers `s.Contains 'x'` under
        // net8.0, where the char overload exists, and that does not compile for
        // a netstandard2.0 target which lacks it. Analysing the narrowest
        // surface keeps every fix valid for the wider ones. (Measured: a probe
        // listing net8.0 first got exactly that break.)
        //
        // ...unless the caller named one. Code behind another framework's #if
        // is invisible to the narrowest analysis — it is not in the parse tree
        // at all — so reaching it means asking for that framework by name.
        let targetFramework =
            let m =
                Text.RegularExpressions.Regex.Match(projectText, "<TargetFrameworks>([^<]+)</TargetFrameworks>")

            if chosenFramework <> "" then
                Some chosenFramework
            elif m.Success then
                m.Groups.[1].Value.Split ';'
                |> Array.map _.Trim()
                |> Array.filter (fun tfm -> tfm <> "")
                |> Array.sortBy tfmRank
                |> Array.tryHead
            else
                None

        let tfmArg =
            match targetFramework with
            | Some tfm ->
                printfn $"  (multi-targeted; analysing against {tfm})"
                $" -p:TargetFramework={tfm}"
            | None -> ""

        let runner, prefix =
            if isSdkStyle then
                "dotnet", ""
            else
                // best effort without VS installed
                vsMsBuildPath.Value
                |> Option.map (fun msbuild -> msbuild, null)
                |> Option.defaultValue ("dotnet", "")

        let run (arguments: string) =
            let finalArgs =
                // dotnet needs the verb ("build"/"msbuild"); MSBuild.exe does not
                if isNull prefix then
                    let firstSpace = arguments.IndexOf ' '

                    if firstSpace > 0 then
                        arguments.Substring(firstSpace + 1)
                    else
                        arguments
                else
                    arguments

            runProcess processTimeout runner finalArgs

        // a REAL build first: project references must exist on disk for the
        // args-only pass below (SkipCompilerExecution skips them too), and a
        // project that does not build has no business being rewritten
        let buildExit, buildOut, buildErr = run $"build \"{projectPath}\"{tfmArg}"

        if buildExit <> 0 then
            // the raw MSBuild transcript buries the compile errors under
            // restore chatter and MSB warnings; show just the error lines
            // (a type provider's connection failure surfaces here too)
            let errorLines =
                ($"{buildOut}\n{buildErr}").Split '\n'
                |> Array.filter (fun l -> l.Contains "error")
                |> Array.distinct
                |> Array.truncate 8

            let detail =
                if errorLines.Length = 0 then
                    $"{buildOut}\n{buildErr}"
                else
                    String.concat "\n" errorLines

            Error $"dotnet build failed — fix the build before applying fixes:\n{detail}"
        else
            // Rebuild forces CoreCompile even when the build above left the
            // project up-to-date (an incremental skip yields no args at all);
            // BuildProjectReferences=false keeps the referenced outputs intact.
            // With SkipCompilerExecution the target fails AFTER emitting the
            // args (no dll to copy) — judge by the JSON, not the exit code.
            let exit, stdout, stderr =
                run
                    $"msbuild \"{projectPath}\" -t:Rebuild -p:BuildProjectReferences=false -p:ProvideCommandLineArgs=true -p:SkipCompilerExecution=true --getItem:FscCommandLineArgs{tfmArg}"

            try
                use doc = JsonDocument.Parse stdout

                let args =
                    doc.RootElement.GetProperty("Items").GetProperty("FscCommandLineArgs").EnumerateArray()
                    |> Seq.map (fun item -> item.GetProperty("Identity").GetString())
                    |> Array.ofSeq

                if args.Length = 0 then
                    Error "MSBuild produced no FscCommandLineArgs (is this an SDK-style F# project?)"
                else
                    Ok args
            with :? JsonException ->
                Error $"dotnet msbuild (exit {exit}) produced no readable args:\n{stdout}\n{stderr}"

/// All [<CliAnalyzer>]-attributed functions of the analyzers assembly.
let private cliAnalyzers () =
    let assembly = typeof<FSharp.Refactor.RedundantParens.Suggestion>.Assembly

    [ for t in assembly.GetTypes() do
          for m in t.GetMethods(BindingFlags.Static ||| BindingFlags.Public) do
              if m.GetCustomAttributes(typeof<CliAnalyzerAttribute>, false).Length > 0 then
                  m ]

/// Analyzers whose CLI wrappers never touch CheckFileResults — the set a
/// --parse-only run may execute against an unresolvable compilation. The
/// typed analyzers are excluded outright rather than trusted to
/// self-silence: their conservative gates assume a compilation that at
/// least TRIED to resolve. Curated against the wrapper bodies in
/// Analyzers.fs; a new syntactic analyzer earns its entry here.
let private parseOnlySafeAnalyzers =
    set
        [ "AbbreviatedType"
          "CommentDoc"
          "LiterateComment"
          "ArgNames"
          "AttributeMerge"
          "AutoProperty"
          "BooleanSimplify"
          "CeStrip"
          "CheckedArithmetic"
          "Composition"
          "ConversionMove"
          "DuFieldNames"
          "ExceptionRules"
          "FormatArgs"
          "Hints"
          "IndexedLoop"
          "InterpToString"
          "LambdaBuiltin"
          "LoopPerf"
          "MatchBang"
          "MatchToIf"
          "MethodCallParens"
          "MiscRules"
          "ObjectRules"
          "PathSeparator"
          "PatternParens"
          "RaiseFailwith"
          "RecursiveAppend"
          "RecursiveSeq"
          "RedundantParens"
          "RedundantSyntax"
          "RegexUsage"
          "LiteralConst"
          "MatchArmMerge"
          "MatchGuards"
          "ObsoleteCrypto"
          "RecGroup"
          "RegexValidity"
          "SecretLiterals"
          "SecurityRules"
          "StructDu"
          "StructHints"
          "SwallowedException"
          "TabIndentation"
          "TaskStateMachine"
          "TrailingSemicolon"
          "TypeChecks"
          "TypeParens"
          "TypeTestChain"
          "UnicodeHygiene"
          "UnimplementedBranch"
          "WhileBang"
          "XmlDocParams" ]

let private analyzerName (m: MethodInfo) =
    (m.GetCustomAttributes(typeof<CliAnalyzerAttribute>, false).[0] :?> CliAnalyzerAttribute).Name

/// The encoding a source file is written in, judged by its BOM — so an
/// edit does not silently strip a UTF-8 BOM or re-encode a UTF-16 file.
let private encodingOf (path: string) : System.Text.Encoding =
    let bom =
        try
            use fs = File.OpenRead path
            let buffer = Array.zeroCreate 3
            let n = fs.Read(buffer, 0, 3)
            Array.truncate n buffer
        with
        | :? IOException
        | :? UnauthorizedAccessException -> [||]

    match bom with
    | [| 0xEFuy; 0xBBuy; 0xBFuy |] -> System.Text.UTF8Encoding true
    | _ when bom.Length >= 2 && bom.[0] = 0xFFuy && bom.[1] = 0xFEuy -> System.Text.Encoding.Unicode
    | _ when bom.Length >= 2 && bom.[0] = 0xFEuy && bom.[1] = 0xFFuy -> System.Text.Encoding.BigEndianUnicode
    | _ -> System.Text.UTF8Encoding false

/// Write a source file back in the encoding it already had.
let private writeSource (path: string) (text: string) =
    File.WriteAllText(path, text, encodingOf path)

/// Set for the run by executeRun. In --parse-only mode nothing resolves,
/// so only PARSE-phase diagnostics are meaningful: a fix that spells a
/// new identifier (`CultureInfo.InvariantCulture`) adds unresolved-
/// reference errors to the pile, and a raw count comparison then blamed
/// the fix for pre-existing noise (found on PethostBackup: every FR0067
/// culture fix rolled back for inflating FS0039s that were ignored to
/// begin with).
let mutable internal parseOnlyRun = false

let private projectErrors (checker: FSharpChecker) (options: FSharpProjectOptions) =
    let results = checker.ParseAndCheckProject options |> Async.RunSynchronously

    results.Diagnostics
    |> Array.filter (fun d ->
        d.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error
        && (not parseOnlyRun || d.Subcategory = "parse"))

let private errorCount (checker: FSharpChecker) (options: FSharpProjectOptions) = (projectErrors checker options).Length

/// Apply grouped edits, bottom-up per file, skipping any fix overlapping
/// one already taken; the original text is verified before each splice.
/// A rule's kind, padded so the file paths after it line up. "correctness"
/// and "performance" are the longest at eleven.
let private kindColumn (code: string) =
    let kind = RuleCatalog.name (RuleCatalog.categoryOf code)
    $"[{kind}]".PadRight 13

/// A file one pass changed: its path, its pre-pass text, and the fixes
/// that landed in it — enough to undo the pass's work on the file and to
/// suppress those fixes on later passes.
type private AppliedFile =
    { Path: string
      Before: string
      Fixes: (string * Fix) list }

/// The suppression key of a fix: rule code, file, and the edit's CONTENT.
/// Not coordinates — a later pass applying an unrelated fix ABOVE the
/// suppressed spot shifts every line below it, the coordinate key misses,
/// and the re-applied fix triggers another rollback, oscillating until
/// --max-passes. Content keys survive shifting; the cost is suppressing an
/// identical same-rule fix elsewhere in the same file, which — having
/// identical content — would almost certainly have broken identically.
let private fixKey (code: string) (file: string) (f: Fix) =
    code, Path.GetFullPath file, f.FromText, f.ToText

/// Returns the number of fixes applied and the files they changed.
/// `suppressed` holds fixes rolled back by an earlier pass's verification;
/// re-applying one would only be rolled back again.
///
/// Edits carry a GROUP id — one per suggestion — and a group applies all
/// or nothing within a file. A hoist is an insertion plus a deletion:
/// dropping the insertion (say, to an overlap with another suggestion at
/// the same point) while the deletion lands removes a binding its uses
/// still need. Found live on Fuuga: two FR0071 hoists inserting at one
/// point, half of the second applied, `startBlock` undefined.
let private applyEditGroups
    (dryRun: bool)
    (suppressed: System.Collections.Generic.HashSet<string * string * string * string>)
    (editsByFile: System.Collections.Generic.Dictionary<string, ResizeArray<int * string * Fix>>)
    : int * AppliedFile list =
    let mutable applied = 0
    let appliedFiles = ResizeArray<AppliedFile>()

    for kv in editsByFile do
        let file = kv.Key
        let text = File.ReadAllText file

        // bottom-up, so earlier splices never shift later ranges
        let edits =
            kv.Value
            |> Seq.sortByDescending (fun (_, _, f) -> f.FromRange.StartLine, f.FromRange.StartColumn)
            |> List.ofSeq

        let groupEdits =
            kv.Value
            |> Seq.groupBy (fun (g, _, _) -> g)
            |> Map.ofSeq
            |> Map.map (fun _ es -> List.ofSeq es)

        let mutable current = text
        let mutable appliedRanges: Range list = []
        let mutable appliedHere: (string * Fix) list = []
        let groupDecisions = System.Collections.Generic.Dictionary<int, bool>()

        let overlaps (r: Range) =
            appliedRanges
            |> List.exists (fun a ->
                Range.rangeContainsPos a r.Start
                || Range.rangeContainsPos a r.End
                || Range.rangeContainsRange r a)

        // can this edit be spliced into `current` exactly as promised?
        // (start/end computed against original coordinates, which stay
        // valid above every already-applied splice in the bottom-up sweep)
        let viable (f: Fix) =
            let lines = current.Split '\n'

            if
                f.FromRange.StartLine - 1 > lines.Length
                || f.FromRange.EndLine - 1 > lines.Length
            then
                None
            else
                let startIndex =
                    (lines
                     |> Seq.take (f.FromRange.StartLine - 1)
                     |> Seq.sumBy (fun l -> l.Length + 1))
                    + f.FromRange.StartColumn

                let endIndex =
                    (lines |> Seq.take (f.FromRange.EndLine - 1) |> Seq.sumBy (fun l -> l.Length + 1))
                    + f.FromRange.EndColumn

                if
                    startIndex <= current.Length
                    && endIndex <= current.Length
                    && current.Substring(startIndex, endIndex - startIndex).Replace("\r", "") = f.FromText.Replace(
                        "\r",
                        ""
                    )
                then
                    Some(startIndex, endIndex)
                else
                    None

        // decide a whole group the moment its bottom-most edit is reached:
        // every member must be unsuppressed, non-overlapping and viable, or
        // none of them applies. A self-identical edit is tolerated inside a
        // group (it changes nothing either way) but sinks a group of one.
        let decideGroup (groupId: int) =
            let members = groupEdits.[groupId]

            let ok =
                members
                |> List.forall (fun (_, code, f) ->
                    not (suppressed.Contains(fixKey code file f))
                    && not (overlaps f.FromRange)
                    && (f.ToText.Replace("\r", "") = f.FromText.Replace("\r", "") || (viable f).IsSome))
                && members
                   |> List.exists (fun (_, _, f) -> f.ToText.Replace("\r", "") <> f.FromText.Replace("\r", ""))

            if ok then
                // reserve every member's range at once, so no other group
                // can interleave between this one's edits
                for _, _, f in members do
                    appliedRanges <- f.FromRange :: appliedRanges
            elif members.Length > 1 then
                let _, code, f = List.head members

                printfn
                    $"  {code} {kindColumn code} {Path.GetFileName file}({f.FromRange.StartLine},{f.FromRange.StartColumn}): held back (its edits cannot all apply together)"

            groupDecisions.[groupId] <- ok
            ok

        for groupId, code, f in edits do
            let accepted =
                match groupDecisions.TryGetValue groupId with
                | true, decision -> decision
                | false, _ -> decideGroup groupId

            let changesSomething = f.ToText.Replace("\r", "") <> f.FromText.Replace("\r", "")

            if accepted && changesSomething then
                match viable f with
                | Some(startIndex, endIndex) ->
                    // splice in the file's own line-ending convention, so
                    // an LF replacement does not seed a CRLF file with
                    // mixed endings
                    let eol = if current.Contains "\r\n" then "\r\n" else "\n"
                    let toText = f.ToText.Replace("\r\n", "\n").Replace("\n", eol)

                    current <- current.Remove(startIndex, endIndex - startIndex).Insert(startIndex, toText)
                    appliedHere <- (code, f) :: appliedHere
                    applied <- applied + 1

                    printfn
                        $"  {code} {kindColumn code} {Path.GetFileName file}({f.FromRange.StartLine},{f.FromRange.StartColumn})"
                | None -> ()

        if current <> text && not dryRun then
            writeSource file current

            appliedFiles.Add
                { Path = file
                  Before = text
                  Fixes = appliedHere }

    applied, List.ofSeq appliedFiles

/// One project-wide suggestion, normalized across the API-changing rules:
/// a code, the symbol it rewrites, and edits that may land in any file.
type private ApiSuggestion =
    { Code: string
      FunctionName: string
      Edits: (Range * string * string) list }

/// The API-CHANGING pass: project-wide refactorings whose edits cross file
/// boundaries — currying internal/public tupled functions (FR0090) and
/// reordering their parameters (FR0091), rewriting every call site in the
/// project. Runs only under --api-changes, BEFORE the normal passes so
/// they can polish the result. A project carrying signature files skips
/// the pass: the .fsi would need the same change.
///
/// A suggestion is atomic: its definition edit and every call-site edit
/// apply together or not at all — a call site left in the old shape while
/// the definition changes would not compile. When two suggestions collide
/// (a call to one inside a call tuple of another) the later one is held
/// back whole, and the caller's iteration picks it up on the next round.
let private runApiPass
    (checker: FSharpChecker)
    (options: FSharpProjectOptions)
    (codes: Set<string> option)
    (dryRun: bool)
    (suppressed: System.Collections.Generic.HashSet<string * string * string * string>)
    =
    if options.SourceFiles |> Array.exists (fun f -> f.EndsWith ".fsi") then
        printfn "  (api pass skipped: signature files would need the same changes)"
        0, []
    else
        let wanted (file: string) (code: string) (name: string) =
            codes |> Option.forall (fun allowed -> allowed.Contains code)
            && Configuration.isRuleEnabled file code name

        let projectResults = checker.ParseAndCheckProject options |> Async.RunSynchronously

        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions options

        let fileContexts =
            System.Collections.Generic.Dictionary<string, Text.FileContext>(StringComparer.OrdinalIgnoreCase)

        for file in options.SourceFiles do
            let sourceText = SourceText.ofString (File.ReadAllText file)

            let parsed =
                checker.ParseFile(file, sourceText, parsingOptions) |> Async.RunSynchronously

            fileContexts.[Path.GetFullPath file] <-
                { FileName = file
                  Source = sourceText
                  ParseTree = parsed.ParseTree }

        let fileLookup (name: string) =
            match fileContexts.TryGetValue(Path.GetFullPath name) with
            | true, ctx -> Some ctx
            | false, _ -> None

        let suggestions = ResizeArray<ApiSuggestion>()

        for file in options.SourceFiles do
            let ctx = fileContexts.[Path.GetFullPath file]

            let _, checkAnswer =
                checker.ParseAndCheckFileInProject(file, 0, ctx.Source, options)
                |> Async.RunSynchronously

            match checkAnswer with
            | FSharpCheckFileAnswer.Succeeded checkResults ->
                if wanted file "FR0090" "TupleParams" then
                    for s in TupleParams.findApiChanges ctx checkResults projectResults fileLookup do
                        suggestions.Add
                            { Code = "FR0090"
                              FunctionName = s.FunctionName
                              Edits = s.Edits |> List.map (fun e -> e.Range, e.Original, e.Replacement) }

                if wanted file "FR0091" "ParamOrder" then
                    for s in ParamOrder.findApiChanges ctx checkResults projectResults fileLookup do
                        suggestions.Add
                            { Code = "FR0091"
                              FunctionName = s.FunctionName
                              Edits = s.Edits }
            | FSharpCheckFileAnswer.Aborted -> ()

        let editsByFile =
            System.Collections.Generic.Dictionary<string, ResizeArray<int * string * Fix>>(
                StringComparer.OrdinalIgnoreCase
            )

        // one group per suggestion; within a FILE its edits then apply all
        // or nothing (a cross-file suggestion still applies per file)
        let mutable nextGroup = 0

        let acceptedRanges =
            System.Collections.Generic.Dictionary<string, ResizeArray<Range>>(StringComparer.OrdinalIgnoreCase)

        let overlapsAccepted (r: Range) =
            match acceptedRanges.TryGetValue(Path.GetFullPath r.FileName) with
            | true, ranges ->
                ranges
                |> Seq.exists (fun a ->
                    Range.rangeContainsPos a r.Start
                    || Range.rangeContainsPos a r.End
                    || Range.rangeContainsRange r a)
            | false, _ -> false

        for s in suggestions do
            if s.Edits |> List.exists (fun (r, _, _) -> overlapsAccepted r) then
                printfn
                    $"  {s.Code} {kindColumn s.Code} {s.FunctionName}: held back this round (edits nest inside another change)"
            else
                printfn $"  {s.Code} {kindColumn s.Code} {s.FunctionName}: {s.Edits.Length} edit(s) across the project"

                nextGroup <- nextGroup + 1

                for range, original, replacement in s.Edits do
                    let target = Path.GetFullPath range.FileName

                    let fix =
                        { FromRange = range
                          FromText = original
                          ToText = replacement }

                    match editsByFile.TryGetValue target with
                    | true, existing -> existing.Add(nextGroup, s.Code, fix)
                    | false, _ ->
                        let fresh = ResizeArray()
                        fresh.Add(nextGroup, s.Code, fix)
                        editsByFile.[target] <- fresh

                    match acceptedRanges.TryGetValue target with
                    | true, ranges -> ranges.Add range
                    | false, _ ->
                        let fresh = ResizeArray()
                        fresh.Add range
                        acceptedRanges.[target] <- fresh

        applyEditGroups dryRun suppressed editsByFile

/// One analyze-and-apply pass over every file. Returns the number of fixes
/// applied.
/// The project's OWN guard constant for dual-framework capability fixes:
/// a DefineConstants value whose $(TargetFramework) conditions cover
/// modern frameworks and none of the legacy ones — SQLProvider's
///
///     <DefineConstants Condition=" '$(TargetFramework)' == 'netstandard2.1'
///         Or '$(TargetFramework)' == 'net8.0' ...">NETSTANDARD21</DefineConstants>
///
/// says "NETSTANDARD21 marks the net6+-capable half", so #if blocks are
/// written in the project's own vocabulary. Both condition placements are
/// read (on the element, or on its PropertyGroup); only conditions built
/// purely from '$(TargetFramework)' == '...' comparisons joined by Or are
/// trusted — anything fancier is ignored. No usable constant means no
/// dual emission at all: nothing is invented.
let private chooseDualConstant (projectPath: string) (modern: string list) (legacy: string list) : string option =
    try
        let text = File.ReadAllText projectPath

        let tfmsOf (condition: string) =
            let comparisons =
                System.Text.RegularExpressions.Regex.Matches(condition, "'\\$\\(TargetFramework\\)'\\s*==\\s*'([^']+)'")

            if comparisons.Count = 0 || condition.Contains "!=" then
                None
            else
                // strip the recognized comparisons; only Or, parens and
                // whitespace may remain, or the condition is too clever
                let stripped =
                    System.Text.RegularExpressions.Regex.Replace(
                        condition,
                        "'\\$\\(TargetFramework\\)'\\s*==\\s*'[^']+'",
                        ""
                    )

                let leftovers =
                    System.Text.RegularExpressions.Regex.Replace(stripped, "(?i)\\bOr\\b|[()\\s]", "")

                if leftovers = "" then
                    Some [ for m in comparisons -> m.Groups.[1].Value ]
                else
                    None

        let definedOn =
            System.Collections.Generic.Dictionary<string, System.Collections.Generic.HashSet<string>>()

        // constants also defined SOMEWHERE we cannot resolve to a framework
        // set — an unconditioned <DefineConstants>$(DefineConstants);FIREBIRD</...>,
        // a Configuration condition, anything clever. Those may be active on
        // legacy frameworks too, so they are disqualified outright: when the
        // scope cannot be known, the constant is not a candidate
        let tainted = System.Collections.Generic.HashSet<string>()

        for groupMatch in propertyGroupRegex.Matches text do
            let groupCondition = conditionAttributeRegex.Match groupMatch.Groups.[1].Value

            for defineMatch in defineElementRegex.Matches groupMatch.Groups.[2].Value do
                let elementCondition = conditionAttributeRegex.Match defineMatch.Groups.[1].Value

                let condition =
                    if elementCondition.Success then
                        elementCondition.Groups.[1].Value
                    elif groupCondition.Success then
                        groupCondition.Groups.[1].Value
                    else
                        ""

                let constants =
                    [ for c in defineMatch.Groups.[2].Value.Split ';' do
                          let c = c.Trim()

                          if c <> "" && not (c.Contains "$(") then
                              c ]

                match (if condition = "" then None else tfmsOf condition) with
                | None ->
                    for constant in constants do
                        tainted.Add constant |> ignore
                | Some tfms ->
                    for constant in constants do
                        match definedOn.TryGetValue constant with
                        | true, existing -> existing.UnionWith tfms
                        | false, _ -> definedOn.[constant] <- System.Collections.Generic.HashSet tfms

        // only framework-SHAPED names qualify (NETSTANDARD21, NET8, net80):
        // a flavor constant like MICROSOFTSQL or LOGARY5 may share the exact
        // TFM condition in this fsproj, but its meaning is the flavor, and a
        // sibling project compiling the same shared file can define it
        // unconditionally — legacy included — turning our #if into a break.
        // The trailing digits are required: SQLProvider's bare NETSTANDARD
        // is a fossil of netstandard-vs-net451 days and is nowadays defined
        // everywhere
        let frameworkShaped =
            System.Text.RegularExpressions.Regex @"^(?i)net(standard|coreapp)?[\d_]+$"

        // a name that DENOTES a legacy framework (NET48, NET451, NET481,
        // NETSTANDARD2_0...) can never guard the modern branch, whatever
        // its fsproj conditions say: the SDK implicitly defines exactly
        // that constant during the legacy compilation itself, invisibly
        // to this textual parse, and #if NET48 would then flip the modern
        // branch ON for net48
        let legacyNamed =
            System.Text.RegularExpressions.Regex @"^(?i)(net4\d*|netstandard1[\d_]*|netstandard2_?0|netcoreapp2_?0)$"

        definedOn
        |> Seq.filter (fun kv ->
            // never defined for a legacy framework, never defined anywhere
            // unknowable, framework-shaped without naming a legacy one, and
            // active for at least one of this project's modern frameworks
            not (tainted.Contains kv.Key)
            && frameworkShaped.IsMatch kv.Key
            && not (legacyNamed.IsMatch kv.Key)
            && legacy |> List.forall (kv.Value.Contains >> not)
            && modern |> List.exists kv.Value.Contains)
        |> Seq.sortByDescending (fun kv -> modern |> List.filter kv.Value.Contains |> List.length)
        |> Seq.tryHead
        |> Option.map (fun kv -> kv.Key)
    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
        None

/// Do any of the project's sources use conditional compilation? Parsed
/// from the fsproj's own Compile items — cheap, and it fails TOWARD
/// caution: wildcards, imports or an unreadable file all report true, so
/// the full framework-by-framework sweep still happens. Only a plainly
/// #if-free project earns the single-framework fast path (its other
/// frameworks are still verified by the final all-frameworks build).
let private sourcesUseConditionals (projectPath: string) =
    try
        let dir = Path.GetDirectoryName(Path.GetFullPath projectPath)
        let projText = File.ReadAllText projectPath

        let includes =
            System.Text.RegularExpressions.Regex.Matches(projText, "Compile\\s+Include=\"([^\"]+)\"")
            |> Seq.map (fun m -> m.Groups.[1].Value)
            |> List.ofSeq

        if includes.IsEmpty || includes |> List.exists (fun i -> i.Contains '*') then
            true // items come from elsewhere or globs: assume conditionals
        else
            includes
            |> List.exists (fun rel ->
                let path = Path.Combine(dir, rel)
                not (File.Exists path) || (File.ReadAllText path).Contains "#if")
    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
        true

/// (lowercased full path, conditional-defines key) pairs already swept in
/// THIS run. Shared-source solutions compile the same file into many
/// projects; under identical defines the parse tree is identical, its
/// fixes are already applied, and re-sweeping it is pure cost. Registered
/// per file only when its TARGET completes (intra-target passes must
/// re-sweep, because a pass-1 fix can enable a pass-2 one); cleared at
/// the start of each run.
let private sweptFiles = System.Collections.Generic.HashSet<string * string>()

/// Fixes applied anywhere in this run so far — the whole-compilation skip
/// is only sound while the tree is untouched (or the run is a dry run).
let mutable internal runTotalApplied = 0

/// A source file with no `#if` in it parses identically under EVERY
/// define set, so one sweep covers all frameworks and all projects — the
/// key degrades to "". Files carrying directives keep the exact-defines
/// key. Cached: solutions ask per project.
///
/// Documented limit: identical parse tree does not mean identical TYPED
/// findings — a sibling file's `#if` or a project's different references
/// can change what a typed rule sees. The trade is deliberate: those
/// deltas are rare, the narrowest-first ordering analyses the most
/// restrictive context first, and the alternative is the full N×TFM
/// re-sweep this dedup exists to remove.
let private directiveFreeCache =
    System.Collections.Concurrent.ConcurrentDictionary<string, bool>()

let private isDirectiveFree (path: string) =
    directiveFreeCache.GetOrAdd(
        path,
        fun p ->
            try
                File.ReadLines p
                |> Seq.forall (fun l -> not (l.TrimStart().StartsWith "#if"))
            with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                false
    )

let private fileSweepKey (definesKeyStr: string) (path: string) =
    if isDirectiveFree path then "" else definesKeyStr

/// Every finding the run surfaced, for the --report SARIF file: fixable or
/// not, applied or held back. Deduplicated because passes re-analyze and a
/// multi-targeted project sweeps the same file once per framework.
/// One surfaced finding, carrying everything a report (or an agent)
/// needs without re-opening the file.
type ReportedFinding =
    {
        File: string
        Code: string
        Message: string
        Severity: Severity
        StartLine: int
        StartColumn: int
        EndLine: int
        EndColumn: int
        /// Does a quick fix exist, or is this a note?
        Fixable: bool
        /// Stable identity across line shifts and sessions: a hash of the
        /// rule code, the file's NAME (not path — checkouts differ), and the
        /// whitespace-normalized source lines around the finding. Two
        /// sessions looking at the same code agree on it; an unrelated edit
        /// elsewhere in the file does not change it.
        Fingerprint: string
        /// The finding's own line(s) with one line of margin.
        Snippet: string
    }

/// The fingerprint version key used in SARIF partialFingerprints.
[<Literal>]
let private FingerprintKey = "fsrefContextHash/v1"

let private fingerprintAndSnippet (source: ISourceText) (file: string) (code: string) (r: range) =
    let lineCount = source.GetLineCount()
    let clamp l = max 0 (min (lineCount - 1) l)
    let firstContext = clamp (r.StartLine - 3)
    let lastContext = clamp (r.EndLine + 1)

    let normalized =
        [ for l in firstContext..lastContext -> whitespaceRunRegex.Replace(source.GetLineString(l).Trim(), " ") ]
        |> String.concat "\n"

    let hash =
        use sha = System.Security.Cryptography.SHA256.Create()

        let bytes =
            System.Text.Encoding.UTF8.GetBytes($"{code}|{Path.GetFileName(file).ToLowerInvariant()}|{normalized}")

        (sha.ComputeHash bytes)[..7] |> Array.map (sprintf "%02x") |> String.concat ""

    let snippet =
        [ for l in clamp (r.StartLine - 2) .. clamp r.EndLine -> source.GetLineString l ]
        |> String.concat "\n"

    hash, snippet

/// Fingerprints an earlier run accepted (--baseline): findings matching
/// them are neither reported nor fixed this run.
let mutable private baselineFingerprints: Set<string> = Set.empty
let mutable private baselineSuppressed = 0

/// Findings a suppression comment silenced this run — never silent
/// silence: the run summary counts them.
let mutable private commentSuppressed = 0

/// Suppression comments the config's "suppressions" policy declined to
/// honor: their findings were reported anyway (though never auto-fixed).
let mutable private suppressionOverridden = 0

/// --honor-suppressions: comments silence everything regardless of the
/// config policy — the CI override.
let mutable private honorAllSuppressions = false

/// --notes: list fix-less advisory notes inline. Off by default — the
/// fixes are the product; held notes are counted per category and
/// summarized at the end (SARIF/JSON always carry them in full).
let mutable private showNotes = false

/// Category name -> held note count for the run summary.
let private heldNoteCounts = System.Collections.Generic.Dictionary<string, int>()

let private reportedFindings = ResizeArray<ReportedFinding>()

let private reportedKeys = System.Collections.Generic.HashSet<string>()

/// Console notes already shown this run — later passes recompute the same
/// fix-less findings, and printing them once is enough.
let private printedNotes = System.Collections.Generic.HashSet<string>()

/// Every comment in a parse tree, as (range, text) — the guard against
/// fixes that would silently swallow one.
let private commentsIn (parseTree: ParsedInput) (source: ISourceText) =
    let ranges =
        match parseTree with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = trivia)) -> trivia.CodeComments
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = trivia)) -> trivia.CodeComments
        |> List.map (fun c ->
            match c with
            | CommentTrivia.LineComment r
            | CommentTrivia.BlockComment r -> r)

    ranges |> List.map (fun r -> r, textOfRange source r)

/// Minimal SARIF 2.1.0, hand-built with System.Text.Json: enough for
/// GitHub code scanning to render inline annotations, no Sarif.Sdk
/// dependency. Paths are relativized against the working directory when
/// they fall under it — that is what code-scanning matches blobs by.
let private writeSarifReport (path: string) (findings: ReportedFinding seq) =
    let root = Directory.GetCurrentDirectory().Replace('\\', '/').TrimEnd('/') + "/"

    let uriOf (file: string) =
        let full = Path.GetFullPath(file).Replace('\\', '/')

        if full.StartsWith(root, StringComparison.OrdinalIgnoreCase) then
            full.Substring root.Length
        else
            // an absolute path is not a valid SARIF uri; a file outside the
            // working directory gets the absolute scheme instead
            Uri(Path.GetFullPath file).AbsoluteUri

    let levelOf severity =
        match severity with
        | Severity.Error -> "error"
        | Severity.Warning -> "warning"
        | Severity.Info
        | Severity.Hint -> "note"

    let results =
        [ for f in findings ->
              dict
                  [ "ruleId", box f.Code
                    "level", box (levelOf f.Severity)
                    "message", box (dict [ "text", box f.Message ])
                    // stable across line shifts and sessions; the baseline
                    // mechanism keys on this
                    "partialFingerprints", box (dict [ FingerprintKey, box f.Fingerprint ])
                    "properties", box (dict [ "fixable", box f.Fixable ])
                    "locations",
                    box
                        [ dict
                              [ "physicalLocation",
                                box (
                                    dict
                                        [ "artifactLocation", box (dict [ "uri", box (uriOf f.File) ])
                                          "region",
                                          box (
                                              dict
                                                  [ "startLine", box (max 1 f.StartLine)
                                                    "startColumn", box (f.StartColumn + 1)
                                                    "endLine", box (max 1 f.EndLine)
                                                    "endColumn", box (f.EndColumn + 1)
                                                    // saves the reader (human
                                                    // or agent) one file-open
                                                    // per finding
                                                    "snippet", box (dict [ "text", box f.Snippet ]) ]
                                          ) ]
                                ) ] ] ] ]

    let report =
        dict
            [ "$schema", box "https://json.schemastore.org/sarif-2.1.0.json"
              "version", box "2.1.0"
              "runs",
              box
                  [ dict
                        [ "tool",
                          box (
                              dict
                                  [ "driver",
                                    box (
                                        dict
                                            [ "name", box "fsharp-refactor"
                                              "informationUri", box "https://github.com/Thorium/FSharp.Refactorings" ]
                                    ) ]
                          )
                          "results", box results ] ] ]

    File.WriteAllText(path, JsonSerializer.Serialize(report, JsonSerializerOptions(WriteIndented = true)))

let private recordForReport (finding: ReportedFinding) =
    lock reportedFindings (fun () ->
        let key =
            $"{finding.Code}|{finding.File}|{finding.StartLine}|{finding.StartColumn}|{finding.Message}"

        if reportedKeys.Add key then
            reportedFindings.Add finding)

/// The conditional-compilation identity of a compilation: its sorted
/// --define set. Same file, same defines — same tree.
let private definesKey (options: FSharpProjectOptions) =
    options.OtherOptions
    |> Array.filter (fun o -> o.StartsWith "--define:")
    |> Array.sort
    |> String.concat ";"

/// Mark every non-ignored source of a completed target as swept.
let private markSwept (options: FSharpProjectOptions) =
    let key = definesKey options

    for f in options.SourceFiles do
        if not (Configuration.isIgnoredPath f) then
            sweptFiles.Add(Path.GetFullPath(f).ToLowerInvariant(), fileSweepKey key f)
            |> ignore

let private runPass
    (checker: FSharpChecker)
    (options: FSharpProjectOptions)
    (analyzers: MethodInfo list)
    (codes: Set<string> option)
    (dryRun: bool)
    (apiChanges: bool)
    (jobs: int)
    (onlyFile: string option)
    (suppressed: System.Collections.Generic.HashSet<string * string * string * string>)
    =
    let projectSw = Stopwatch.StartNew()
    let projectResults = checker.ParseAndCheckProject options |> Async.RunSynchronously
    projectSw.Stop()

    // where the wall clock goes, reported at the end of the pass: on a large
    // project typechecking dominates, and knowing that stops people hunting
    // for a slow rule that is not there
    let mutable checkMs = 0L
    let analyzerMs = System.Collections.Generic.Dictionary<string, int64>()

    // every accepted fix, grouped by the file it EDITS: a fix range names
    // its file, so a cross-file (API-changing) rule can edit call sites
    // anywhere in the project — but only under --api-changes. Keys are
    // normalized full paths: two spellings of one file must land in ONE
    // group, or the second write would clobber the first group's edits.
    let editsByFile =
        System.Collections.Generic.Dictionary<string, ResizeArray<int * string * Fix>>(StringComparer.OrdinalIgnoreCase)

    // one group per suggestion (message): its edits apply all or nothing
    let mutable nextGroup = 0

    let mutable crossFileSkipped = 0
    let mutable commentHeldBack = 0

    // files FCS could not check cleanly: most rules stay silent on those, so
    // without this a run over a project that does not typecheck would just
    // report nothing and look clean
    let mutable filesWithErrors = 0

    // Typecheck and analyze each file independently, then aggregate in file
    // order. Everything the analyzers share is already safe to touch from
    // several threads (AstIndex keys a ConditionalWeakTable by parse tree,
    // Configuration and HintEngine cache in ConcurrentDictionaries), and
    // FSharpChecker supports concurrent calls. Nothing is written here — the
    // edits are applied afterwards, sequentially.
    let analyzeFile (file: string) =
        async {
            let sourceText = SourceText.ofString (File.ReadAllText file)
            let checkSw = Stopwatch.StartNew()
            let! parseResults, checkAnswer = checker.ParseAndCheckFileInProject(file, 0, sourceText, options)
            checkSw.Stop()

            match checkAnswer with
            | FSharpCheckFileAnswer.Succeeded checkResults ->
                let context: CliContext =
                    { FileName = file
                      SourceText = sourceText
                      ParseFileResults = parseResults
                      CheckFileResults = checkResults
                      TypedTree = checkResults.ImplementationFile
                      CheckProjectResults = projectResults
                      ProjectOptions = AnalyzerProjectOptions.BackgroundCompilerOptions options
                      // `// fsharpanalyzer: ignore-line FR0031` and friends
                      // (ignore-line-next, ignore-file, ignore-region-start/
                      // end) — the SDK's own suppression comments, honored
                      // here exactly as editors honor them
                      AnalyzerIgnoreRanges = Ignore.getAnalyzerIgnoreRanges parseResults sourceText }

                let timings = ResizeArray<string * int64>()
                let collected = ResizeArray<Message>()

                // bound each analyzer's Async rather than blocking on it:
                // with several files in flight, an Async.RunSynchronously
                // here would tie up a thread-pool thread per job (our own
                // FR0049 flags exactly this, and caught it here)
                for m in analyzers do
                    let sw = Stopwatch.StartNew()

                    let! produced =
                        async {
                            try
                                let work = m.Invoke(null, [| box context |]) :?> Async<Message list>
                                return! work
                            with
                            // an analyzer that throws must not take the run
                            // down, but say so: a silently skipped rule looks
                            // like a clean file
                            | :? TargetInvocationException as ex ->
                                eprintfn $"  (analyzer {m.Name} failed: {ex.InnerException.Message})"
                                return []
                            | :? InvalidCastException as ex ->
                                eprintfn $"  (analyzer {m.Name} has an unexpected signature: {ex.Message})"
                                return []
                        }

                    sw.Stop()
                    timings.Add(m.Name, sw.ElapsedMilliseconds)
                    collected.AddRange produced

                // a suppressed finding is neither reported nor FIXED — for
                // an apply tool the second half is the important one. Same
                // semantics as the SDK's own filter (which its signature
                // file keeps internal), so editors and this tool agree on
                // what a suppression comment silences
                let isSuppressed (msg: Message) =
                    match context.AnalyzerIgnoreRanges |> Map.tryFind msg.Code with
                    | None -> false
                    | Some ranges ->
                        ranges
                        |> List.exists (function
                            | AnalyzerIgnoreRange.File -> true
                            | AnalyzerIgnoreRange.Range(commentStart, commentEnd) ->
                                msg.Range.StartLine - 1 >= commentStart && msg.Range.EndLine - 1 <= commentEnd
                            | AnalyzerIgnoreRange.NextLine line -> msg.Range.StartLine - 1 = line
                            | AnalyzerIgnoreRange.CurrentLine line -> msg.Range.StartLine = line)

                let toFinding (msg: Message) =
                    let findingFile =
                        if String.IsNullOrEmpty msg.Range.FileName then
                            file
                        else
                            msg.Range.FileName

                    let fingerprint, snippet =
                        fingerprintAndSnippet sourceText findingFile msg.Code msg.Range

                    { File = findingFile
                      Code = msg.Code
                      Message = msg.Message
                      Severity = msg.Severity
                      StartLine = msg.Range.StartLine
                      StartColumn = msg.Range.StartColumn
                      EndLine = msg.Range.EndLine
                      EndColumn = msg.Range.EndColumn
                      Fixable = not msg.Fixes.IsEmpty
                      Fingerprint = fingerprint
                      Snippet = snippet }

                // whether a comment is honored is the team's call — the
                // config's "suppressions" policy; --honor-suppressions is
                // the CI override that says yes to all of them
                let policy =
                    if honorAllSuppressions then
                        "all"
                    else
                        Configuration.suppressionPolicy file

                let honoredByPolicy (msg: Message) =
                    match policy with
                    | "none" -> false
                    | "no-correctness" -> RuleCatalog.categoryOf msg.Code <> RuleCatalog.Category.Correctness
                    | _ -> true

                let byCode =
                    collected
                    |> Seq.filter (fun msg -> codes |> Option.forall (fun wanted -> wanted.Contains msg.Code))
                    |> List.ofSeq

                let suppressedByComment, live = byCode |> List.partition isSuppressed
                let silenced, overridden = suppressedByComment |> List.partition honoredByPolicy

                if not suppressedByComment.IsEmpty then
                    lock reportedFindings (fun () ->
                        commentSuppressed <- commentSuppressed + silenced.Length
                        suppressionOverridden <- suppressionOverridden + overridden.Length)

                // an overridden comment still REPORTS its finding — the
                // policy says a comment cannot silence this category — but
                // never auto-fixes over someone's explicit comment
                let overriddenAsNotes =
                    overridden
                    |> List.map (fun m ->
                        { m with
                            Fixes = []
                            Message = m.Message + " (suppression comment not honored — \"suppressions\" policy)" })

                // baseline last: a finding an earlier accepted run already
                // carried is neither reported nor FIXED — the ratchet only
                // moves on what is new
                let reportable, baselined =
                    live @ overriddenAsNotes
                    |> Seq.map (fun msg -> msg, toFinding msg)
                    |> List.ofSeq
                    |> List.partition (fun (_, f) -> not (baselineFingerprints.Contains f.Fingerprint))

                if not baselined.IsEmpty then
                    lock reportedFindings (fun () -> baselineSuppressed <- baselineSuppressed + baselined.Length)

                for _, finding in reportable do
                    recordForReport finding

                let messages, notes =
                    reportable |> List.map fst |> List.partition (fun msg -> not msg.Fixes.IsEmpty)

                return
                    {| File = file
                       CheckMs = checkSw.ElapsedMilliseconds
                       Timings = List.ofSeq timings
                       HasErrors = OptionModule.hasErrors checkResults
                       Messages = messages
                       Notes = notes
                       Comments = commentsIn parseResults.ParseTree sourceText |}
            | FSharpCheckFileAnswer.Aborted ->
                return
                    {| File = file
                       CheckMs = checkSw.ElapsedMilliseconds
                       Timings = []
                       HasErrors = true
                       Messages = []
                       Notes = []
                       Comments = [] |}
        }

    // naming one source file means analyzing its project — the references
    // and the files before it are what give its names meaning — but
    // sweeping only that file
    let named =
        match onlyFile with
        | Some only ->
            options.SourceFiles
            |> Array.filter (fun f -> String.Equals(Path.GetFullPath f, only, StringComparison.OrdinalIgnoreCase))
        | None -> options.SourceFiles

    // vendored and generated code a compilation nonetheless includes —
    // paket-files above all — is neither analyzed nor typechecked here:
    // fixing someone else's vendored source is churn, and sweeping it in
    // every project that includes it is where multi-project runs go to die
    let filesToSweep = named |> Array.filter (Configuration.isIgnoredPath >> not)

    // files an earlier compilation of this RUN already swept under the
    // same conditional-compilation defines: same defines, same parse tree,
    // same fixes — which are already applied. Shared-source solutions
    // (twenty projects compiling one Common) pay for each file once.
    let alreadySwept, filesToSweep =
        filesToSweep
        |> Array.partition (fun f ->
            sweptFiles.Contains(Path.GetFullPath(f).ToLowerInvariant(), fileSweepKey (definesKey options) f))

    if named.Length - alreadySwept.Length - filesToSweep.Length > 0 then
        printfn $"  ({named.Length - alreadySwept.Length - filesToSweep.Length} ignored-path file(s) skipped)"

    if alreadySwept.Length > 0 then
        printfn $"  ({alreadySwept.Length} shared file(s) already swept in an earlier compilation)"

    printf $"sweeping {filesToSweep.Length} file(s)... "
    Console.Out.Flush()
    let sweepSw = Stopwatch.StartNew()

    // a heartbeat on stderr: on a big project a sweep is half a minute of
    // silence when the files are clean, which reads as a hang. Stderr so
    // that piped/JSON stdout stays intact
    let sweptCount = ref 0

    let progress () =
        let n = System.Threading.Interlocked.Increment sweptCount

        if n % 25 = 0 then
            eprintf $"[{n}/{filesToSweep.Length}] "

    let outcomes =
        filesToSweep
        |> Array.map (fun file ->
            async {
                let! outcome = analyzeFile file
                progress ()
                return outcome
            })
        |> fun work -> Async.Parallel(work, maxDegreeOfParallelism = jobs)
        |> Async.RunSynchronously

    sweepSw.Stop()

    // Async.Parallel preserves input order, so the fix listing stays in file
    // order and a run is reproducible regardless of how the work interleaved
    for outcome in outcomes do
        checkMs <- checkMs + outcome.CheckMs

        if outcome.HasErrors then
            filesWithErrors <- filesWithErrors + 1

        for name, ms in outcome.Timings do
            analyzerMs.[name] <-
                (match analyzerMs.TryGetValue name with
                 | true, existing -> existing
                 | false, _ -> 0L)
                + ms

        // fix-less findings (FR0055, FR0028...) have no edit to apply — the
        // note IS the rule's entire output. Counted once per run; the wall
        // of advice is opt-in: the fixes are the product, and a screen of
        // structural homework after them is an anticlimax nobody reads.
        // --notes lists them, --report/--format json always carry them
        for note in outcome.Notes do
            let key =
                $"{note.Code}|{outcome.File}|{note.Range.StartLine}|{note.Range.StartColumn}|{note.Message}"

            if printedNotes.Add key then
                if showNotes then
                    let firstSentence =
                        let text = note.Message
                        // a bare '.' is not a sentence end — "String.Equals"
                        // must survive the cut
                        let cutAt =
                            [ text.IndexOf ". "; text.IndexOf ".\n"; text.IndexOf '\n' ]
                            |> List.filter (fun i -> i >= 0)

                        match cutAt with
                        | [] -> text
                        | cuts -> text.Substring(0, List.min cuts + 1)

                    printfn
                        $"  {note.Code} {kindColumn note.Code} {Path.GetFileName outcome.File}({note.Range.StartLine},{note.Range.StartColumn}) note: {firstSentence}"
                else
                    let kind = RuleCatalog.name (RuleCatalog.categoryOf note.Code)

                    lock heldNoteCounts (fun () ->
                        heldNoteCounts.[kind] <-
                            (match heldNoteCounts.TryGetValue kind with
                             | true, n -> n
                             | false, _ -> 0)
                            + 1)

        // a fix whose span contains a comment the replacement does not carry
        // would silently DELETE it — a match collapsed to one line takes its
        // Note1/Note2 lines with it. Held back instead: the reader's notes
        // outrank our rewrite
        // MESSAGE-level, not fix-level: a compound fix may MOVE code — a
        // remove whose ToText is empty paired with an insert that carries
        // the text (FR0116's extraction). A comment inside any fix's span
        // is only lost when NO fix of the same message re-emits it
        let losesComment (siblingToTexts: string list) (f: Fix) =
            // only consulted for same-file fixes, so the comment list and
            // the range coordinates already speak about the same file
            outcome.Comments
            |> List.exists (fun (r: range, text: string) ->
                Range.rangeContainsRange f.FromRange r
                && not (siblingToTexts |> List.exists (fun t -> t.Contains text)))

        for msg in outcome.Messages do
            nextGroup <- nextGroup + 1

            for f in msg.Fixes do
                let target =
                    Path.GetFullPath(
                        if String.IsNullOrEmpty f.FromRange.FileName then
                            outcome.File
                        else
                            f.FromRange.FileName
                    )

                let sameFile =
                    String.Equals(target, Path.GetFullPath outcome.File, StringComparison.OrdinalIgnoreCase)

                // a cross-file fix guards against the TARGET file's comments,
                // parsed through ProjectSources — the same rule as same-file
                let losesCrossFileComment () =
                    match ProjectSources.tryParse target with
                    | Some(targetTree, targetSource) ->
                        Text.commentsWithText targetTree targetSource
                        |> List.exists (fun (r, text) ->
                            Range.rangeContainsRange f.FromRange r
                            && not (msg.Fixes |> List.exists (fun sibling -> sibling.ToText.Contains text)))
                    | None -> false

                if
                    (sameFile && losesComment [ for sibling in msg.Fixes -> sibling.ToText ] f)
                    || (not sameFile && apiChanges && losesCrossFileComment ())
                then
                    commentHeldBack <- commentHeldBack + 1

                    printfn
                        $"  {msg.Code} {kindColumn msg.Code} {Path.GetFileName outcome.File}({f.FromRange.StartLine},{f.FromRange.StartColumn}): held back (a comment inside its span would be lost)"
                elif sameFile || apiChanges then
                    match editsByFile.TryGetValue target with
                    | true, existing -> existing.Add(nextGroup, msg.Code, f)
                    | false, _ ->
                        let fresh = ResizeArray()
                        fresh.Add(nextGroup, msg.Code, f)
                        editsByFile.[target] <- fresh
                else
                    crossFileSkipped <- crossFileSkipped + 1

    if crossFileSkipped > 0 then
        printfn $"  ({crossFileSkipped} cross-file fix(es) held back — rerun with --api-changes to apply them)"

    if filesWithErrors > 0 then
        eprintfn
            $"  ({filesWithErrors} of {filesToSweep.Length} file(s) have type errors; most rules stay silent on those)"

    let totalAnalyzerMs = analyzerMs.Values |> Seq.sum

    // the per-file and analyzer figures are summed across threads, so they
    // add up to more than the sweep's wall clock — that gap IS the parallelism
    printfn
        $"  timing: project check {projectSw.ElapsedMilliseconds} ms, file sweep {sweepSw.ElapsedMilliseconds} ms wall"

    printfn $"          (summed across threads: checks {checkMs} ms, analyzers {totalAnalyzerMs} ms)"

    let slowest =
        analyzerMs
        |> Seq.sortByDescending (fun kv -> kv.Value)
        |> Seq.truncate 5
        |> Seq.map (fun kv -> $"{kv.Key} {kv.Value}ms")
        |> String.concat ", "

    if slowest <> "" then
        printfn $"  slowest analyzers: {slowest}"

    applyEditGroups dryRun suppressed editsByFile

/// One compilation to work on. Which kind it is comes from the file
/// extension — the caller never has to say.
[<RequireQualifiedAccess>]
type private Target =
    /// A whole project, or — when a single source file was named — that
    /// project analyzed but only that one file edited.
    | Project of project: string * onlyFile: string option
    | Script of string

/// The project a source file belongs to, searching upwards. A .fs file is
/// not a compilation on its own: it needs the project's references, and F#
/// is order-dependent, so the files before it decide what its names mean.
/// A project that actually lists the file wins over a merely nearer one.
let private owningProject (sourceFile: string) =
    let full = Path.GetFullPath sourceFile
    let name = Path.GetFileName full

    // F# projects list their sources explicitly — the order is part of the
    // language — so "lists this file" is a reliable test rather than a guess
    let lists (project: string) =
        try
            (File.ReadAllText project).Contains name
        with
        | :? IOException
        | :? UnauthorizedAccessException -> false

    let projectsUnder dir =
        FileWalk.files "*.fsproj" dir |> List.ofSeq

    // Widen from the file outwards, searching BENEATH each level too,
    // because a project commonly compiles sources from a sibling folder
    // (`<Compile Include="..\Code\Thing.fs" />`) that walking up alone
    // would miss.
    //
    // Stopping matters more than widening. The repository root is the
    // natural edge: above it lies the rest of the disk, and one level too
    // far means recursively enumerating a whole drive — minutes of I/O for
    // a file that was never going to be there. A level cap backstops the
    // case where there is no repository at all.
    let mutable dir = Path.GetDirectoryName full
    let mutable found = None
    let mutable atEdge = false
    let mutable levels = 0

    while found.IsNone && not atEdge && levels < 4 && not (String.IsNullOrEmpty dir) do
        found <- projectsUnder dir |> List.tryFind lists

        let parent = Path.GetDirectoryName dir

        atEdge <-
            Directory.Exists(Path.Combine(dir, ".git"))
            || String.IsNullOrEmpty parent
            // a drive root: Path.GetDirectoryName "C:\\" is null, but be
            // explicit rather than relying on that
            || parent = Path.GetPathRoot dir

        dir <- parent
        levels <- levels + 1

    found

/// The .fsproj paths a solution lists, resolved against the solution's own
/// directory. `.slnx` is XML; the classic `.sln` has one `Project(...)` line
/// per entry. MSBuild refuses to report compiler arguments for a solution
/// itself, so the tool works through the projects instead.
let private projectsInSolution (solutionPath: string) =
    let dir = Path.GetDirectoryName(Path.GetFullPath solutionPath)
    let text = File.ReadAllText solutionPath

    let paths =
        if solutionPath.EndsWith(".slnx", StringComparison.OrdinalIgnoreCase) then
            Text.RegularExpressions.Regex.Matches(text, "Path\\s*=\\s*\"([^\"]+)\"")
            |> Seq.map (fun m -> m.Groups.[1].Value)
        else
            Text.RegularExpressions.Regex.Matches(text, "\"([^\"]+\\.fsproj)\"")
            |> Seq.map (fun m -> m.Groups.[1].Value)

    paths
    |> Seq.filter (fun p -> p.EndsWith(".fsproj", StringComparison.OrdinalIgnoreCase))
    |> Seq.map (fun p -> Path.GetFullPath(Path.Combine(dir, p.Replace('\\', Path.DirectorySeparatorChar))))
    |> Seq.filter File.Exists
    |> Seq.distinct
    |> List.ofSeq

let private targetOf (path: string) =
    match (Path.GetExtension path).ToLowerInvariant() with
    | ".fsproj" -> Some(Target.Project(path, None))
    | ".fsx"
    | ".fsscript" -> Some(Target.Script path)
    | _ -> None

/// Turn whatever the user pointed at into the list of compilations to run:
/// a project or script directly, every project in a solution, everything a
/// glob matches, or — for a directory — the solution or projects inside it.
let private resolveTargets (raw: string) : Result<Target list, string> =
    let expandGlob (pattern: string) =
        let normalized = pattern.Replace('\\', '/')
        let starIndex = normalized.IndexOf '*'

        let root =
            let head = normalized.Substring(0, starIndex)
            let slash = head.LastIndexOf '/'

            if slash >= 0 then head.Substring(0, slash) else "."

        let leaf = Path.GetFileName normalized

        if Directory.Exists root then
            FileWalk.files leaf root |> List.ofSeq
        else
            []

    let fromDirectory (dir: string) =
        let solutions =
            [ yield! Directory.EnumerateFiles(dir, "*.slnx")
              yield! Directory.EnumerateFiles(dir, "*.sln") ]

        let projects =
            match solutions with
            | solution :: _ -> projectsInSolution solution |> List.map (fun p -> Target.Project(p, None))
            | [] ->
                FileWalk.files "*.fsproj" dir
                |> Seq.map (fun p -> Target.Project(p, None))
                |> List.ofSeq

        // loose scripts are code too: build.fsx and friends never appear in
        // any fsproj, so a directory sweep that stopped at projects silently
        // skipped them. The walker already prunes obj/bin/packages/.git;
        // ignorePaths (paket-files above all) applies on top
        let scripts =
            FileWalk.files "*.fsx" dir
            |> Seq.filter (Configuration.isIgnoredPath >> not)
            |> Seq.map Target.Script
            |> List.ofSeq

        projects @ scripts

    if raw.Contains '*' || raw.Contains '?' then
        match expandGlob raw |> List.choose targetOf with
        | [] -> Error $"'{raw}' matched no .fsproj or .fsx files."
        | targets -> Ok targets
    elif Directory.Exists raw then
        match fromDirectory raw with
        | [] -> Error $"No solution or F# project found in '{raw}'."
        | targets -> Ok targets
    elif not (File.Exists raw) then
        Error $"No such file or directory: {raw}"
    else
        match (Path.GetExtension raw).ToLowerInvariant() with
        | ".sln"
        | ".slnx" ->
            match projectsInSolution raw |> List.map (fun p -> Target.Project(p, None)) with
            | [] -> Error $"'{Path.GetFileName raw}' lists no F# projects."
            | targets -> Ok targets
        | ".slnf" -> Error "Solution filters are not supported; pass the solution or a project."
        // one source file: analyze its project (a .fs needs the project's
        // references, and F# is order-dependent) but edit only this file
        | ".fs"
        | ".fsi" ->
            match owningProject raw with
            | Some project -> Ok [ Target.Project(project, Some(Path.GetFullPath raw)) ]
            | None ->
                Error
                    $"No .fsproj found above '{Path.GetFileName raw}'. A source file is not a compilation on its own — it needs its project for references and file order."
        | _ ->
            match targetOf raw with
            | Some target -> Ok [ target ]
            | None ->
                Error
                    $"Don't know what to do with '{Path.GetFileName raw}' — pass a .fsproj, .fsx, solution, directory or glob."

/// The compilation to analyze, from either input kind.
///
/// A script needs no MSBuild at all — FCS resolves a script's own
/// references, including `#load`ed files, which land in SourceFiles and so
/// get analyzed and fixed alongside the script itself. That also makes
/// --script far quicker than --project, which spends its first half-minute
/// in MSBuild before any analysis starts.
let private optionsFor (checker: FSharpChecker) (parseOnly: bool) (chosenFramework: string) (target: Target) =
    match target with
    | Target.Script script ->
        let path = Path.GetFullPath script
        let sourceText = SourceText.ofString (File.ReadAllText path)

        let options, diagnostics =
            // useFsiAuxLib: scripts run under fsi get the fsi object
            // (fsi.CommandLineArgs and friends); resolving without it
            // reported "'fsi' is not defined" on perfectly good scripts
            checker.GetProjectOptionsFromScript(path, sourceText, assumeDotNetFramework = false, useFsiAuxLib = true)
            |> Async.RunSynchronously

        // a reference the script host could not resolve leaves the script
        // half-typed, and most rules then stay silent; say so rather than
        // reporting a suspiciously clean file
        for d in diagnostics |> List.truncate 5 do
            eprintfn $"  (script reference: {d.Message})"

        Ok options
    | Target.Project(project, _) ->
        // announced BEFORE it starts: this step can take a minute, and a
        // line that only appears afterwards is no help while you are
        // staring at a silent terminal wondering whether it is stuck
        printf (
            if parseOnly then
                "reading sources from the project file... "
            else
                "building and reading compiler arguments... "
        )

        Console.Out.Flush()
        let argsSw = Stopwatch.StartNew()

        let fscResult =
            if parseOnly then
                parseOnlyArgs project
            else
                fscArgs chosenFramework project

        argsSw.Stop()
        printfn $"{argsSw.ElapsedMilliseconds} ms"

        match fscResult with
        | Error message -> Error message
        | Ok args ->
            let projectDir = Path.GetDirectoryName(Path.GetFullPath project)

            // FCS leaves SourceFiles empty for command-line args; partition
            // and rebase the relative paths MSBuild emits ourselves
            let sourceExtensions = [| ".fs"; ".fsi"; ".fsx" |]

            let isSource (arg: string) =
                not (arg.StartsWith '-')
                && sourceExtensions
                   |> Array.exists (fun ext -> arg.EndsWith(ext, StringComparison.OrdinalIgnoreCase))

            // Signing is about emitting an assembly, which analysis never
            // does — but FCS still tries to open the key file, and a
            // relative --keyfile: path it cannot resolve reports as a
            // project error, refusing a project that builds perfectly well.
            let isOutputOnly (arg: string) =
                [ "--keyfile:"; "--delaysign"; "--publicsign"; "--sourcelink:" ]
                |> List.exists (fun flag -> arg.StartsWith(flag, StringComparison.OrdinalIgnoreCase))

            let sources, otherArgs =
                args |> Array.filter (isOutputOnly >> not) |> Array.partition isSource

            let absoluteSources =
                sources
                |> Array.map (fun s ->
                    if Path.IsPathRooted s then
                        s
                    else
                        Path.Combine(projectDir, s))

            Ok
                { checker.GetProjectOptionsFromCommandLineArgs(Path.GetFullPath project, otherArgs) with
                    SourceFiles = absoluteSources }

/// Does this target carry frameworks beyond the one we analyze?
let private isMultiTargeted (target: Target) =
    match target with
    | Target.Script _ -> false
    | Target.Project(project, _) ->
        try
            (File.ReadAllText project).Contains "<TargetFrameworks>"
        with
        | :? IOException
        | :? UnauthorizedAccessException -> false

/// Every framework a project targets, narrowest first.
let private frameworksOf (target: Target) =
    match target with
    | Target.Script _ -> []
    | Target.Project(project, _) ->
        let text =
            try
                File.ReadAllText project
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ""

        let m =
            Text.RegularExpressions.Regex.Match(text, "<TargetFrameworks>([^<]+)</TargetFrameworks>")

        if m.Success then
            m.Groups.[1].Value.Split ';'
            |> Array.map _.Trim()
            |> Array.filter (fun tfm -> tfm <> "")
            |> Array.sortBy tfmRank
            |> List.ofArray
        else
            []

/// Build every framework, so a fix that suits the one we analyzed but not
/// the others cannot pass as success.
let private buildAllFrameworks (project: string) =
    let exitCode, stdout, stderr =
        runProcess processTimeout "dotnet" $"build \"{project}\" --nologo -v q"

    if exitCode = 0 then
        Ok()
    else
        let lines =
            (stdout + stderr).Split '\n'
            |> Array.filter (fun l -> l.Contains "error")
            |> Array.truncate 5

        Error(String.concat "\n" lines)

/// The source files as they stand, so one framework's pass can be undone
/// if it turns out to have broken another's.
let private takeSnapshot (files: string array) =
    files
    |> Array.choose (fun f ->
        try
            Some(f, File.ReadAllText f)
        with
        | :? IOException
        | :? UnauthorizedAccessException -> None)
    |> Map.ofArray

/// Put back every file that changed since the snapshot; returns how many.
let private restoreSnapshot (snapshot: Map<string, string>) =
    snapshot
    |> Map.toSeq
    |> Seq.sumBy (fun (path, original) ->
        try
            if File.ReadAllText path <> original then
                writeSource path original
                1
            else
                0
        with
        | :? IOException
        | :? UnauthorizedAccessException -> 0)

/// Type-check the project after an applying pass; on new errors, roll the
/// pass back — first only the changed files the errors name, then (F#
/// inference being order-dependent, an edit in one file can break a later
/// one) every file the pass changed, which must return the count to zero
/// since the pass started clean. Rolled-back fixes go into `suppressed` so
/// the next pass does not re-apply them and oscillate until --max-passes.
///
/// Nearly free on the happy path: the pass ahead re-uses this check's
/// cached results, so it replaces rather than adds a full project check.
let private verifyPass
    (checker: FSharpChecker)
    (options: FSharpProjectOptions)
    (baselineErrors: int)
    (suppressed: System.Collections.Generic.HashSet<string * string * string * string>)
    (changedFiles: AppliedFile list)
    : bool =
    checker.InvalidateConfiguration options
    let errors = projectErrors checker options

    // measured against the baseline, not zero: a --parse-only run starts
    // with hundreds of unresolved-reference errors that are nobody's fault
    if errors.Length <= baselineErrors then
        true
    else
        let errorFiles =
            errors |> Array.map (fun d -> Path.GetFullPath d.FileName) |> Set.ofArray

        let named =
            changedFiles
            |> List.filter (fun cf -> errorFiles.Contains(Path.GetFullPath cf.Path))

        let restore (files: AppliedFile list) =
            for cf in files do
                writeSource cf.Path cf.Before

                for code, f in cf.Fixes do
                    suppressed.Add(fixKey code cf.Path f) |> ignore

            checker.InvalidateConfiguration options

        let rolledBack =
            if not named.IsEmpty then
                restore named

                if (projectErrors checker options).Length <= baselineErrors then
                    named
                else
                    let rest = changedFiles |> List.except named
                    restore rest
                    changedFiles
            else
                // every error sits in a file this pass never touched. That
                // can still be our doing (an edit's inference ripple), so
                // TEST it: restore, recount — if the errors stay, they were
                // never ours (a vendored file broken for another reason —
                // the SQLProvider paket-files case burned 40 minutes of
                // apply-restore on exactly this), so the fixes go back in
                let currentTexts =
                    changedFiles
                    |> List.map (fun cf -> cf.Path, (try Some(File.ReadAllText cf.Path) with _ -> None)) // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055

                restore changedFiles

                if (projectErrors checker options).Length <= baselineErrors then
                    changedFiles
                else
                    for path, text in currentTexts do
                        match text with
                        | Some t -> writeSource path t
                        | None -> ()

                    checker.InvalidateConfiguration options

                    eprintfn
                        "  (the new errors persist without this pass's fixes — pre-existing breakage elsewhere, fixes kept)"

                    []

        if rolledBack.IsEmpty then
            // the errors survived the un-apply test: not ours, fixes kept
            true
        else
            eprintfn
                "  this pass introduced type errors — its changes were rolled back and the offending fixes suppressed:"

            // the errors themselves, or diagnosing WHICH fix broke means
            // re-running the whole thing by hand
            for d in errors |> Array.truncate 5 do
                eprintfn $"    {Path.GetFileName d.FileName}({d.StartLine},{d.StartColumn}): {d.Message}"

            for cf in rolledBack do
                for code, f in cf.Fixes do
                    eprintfn
                        $"    {code} {Path.GetFileName cf.Path}({f.FromRange.StartLine},{f.FromRange.StartColumn}) rolled back"

            false

/// Analyze and fix one compilation; returns its exit code.
let private runTarget (checker: FSharpChecker) (opts: Options) (showHeader: bool) (target: Target) =
    let onlyFile =
        match target with
        | Target.Project(_, only) -> only
        | Target.Script _ -> None

    // which framework this pass is for, when the project has several
    let frameworkLabel = if opts.Framework = "" then "" else $" [{opts.Framework}]"

    if showHeader then
        let label =
            match target with
            | Target.Project(p, Some file) -> $"{Path.GetFileName file} (in {Path.GetFileName p})"
            | Target.Project(p, None) -> Path.GetFileName p
            | Target.Script s -> Path.GetFileName s

        printfn $"== {label}{frameworkLabel} =="

    match optionsFor checker opts.ParseOnly opts.Framework target with
    | Error message ->
        eprintfn $"{message}"

        // NOT-APPLICABLE targets are not failures: a dacpac project or a
        // wildcard-item fsproj beyond --parse-only was skipped, not broken
        // (a solution containing one used to fail the whole run's exit)
        if message.Contains "— skipped" || message.Contains "beyond --parse-only" then
            0
        else
            1
    | Ok options ->
        let analyzers =
            cliAnalyzers ()
            |> List.filter (fun m -> not opts.ParseOnly || parseOnlySafeAnalyzers.Contains(analyzerName m))

        printfn $"{analyzers.Length} analyzers, {options.SourceFiles.Length} files"

        // cross-file (API-changing) rule variants gate on this: they
        // stay silent in editors and in default runs
        if opts.ApiChanges then
            Environment.SetEnvironmentVariable("FSREF_API_CHANGES", "1")

        // only codes the user TYPED outrank a rule's default-off status and
        // a config disable — asking for FR0099 by name and getting silence
        // would be a lie. A --categories expansion deliberately does NOT
        // qualify: a category is a filter, not an ask, and
        // `--categories idiom` must not quietly turn on FR0002
        match opts.ExplicitCodes with
        | Some codes -> Environment.SetEnvironmentVariable("FSREF_FORCE_CODES", String.concat "," codes)
        | None -> ()

        // Not worth skipping on a dry run: measured, the cost simply
        // moves to runPass's own ParseAndCheckProject, which is only
        // cheap here BECAUSE this call warmed FCS. One full project
        // typecheck is paid either way; this ordering at least reports
        // a broken project up front.
        // before anything is written, so a pass that turns out to break the
        // build — this framework's or another's — can be undone rather than
        // merely reported
        // cross-file migrations (internal FR0069/FR0093 under --api-changes)
        // classify uses in OTHER files against those files' parse trees;
        // this host supplies them, under THIS compilation's defines
        (let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions options

         ProjectSources.configure (
             Some(fun path ->
                 try
                     let sourceText = SourceText.ofString (File.ReadAllText path)

                     let parseResults =
                         checker.ParseFile(path, sourceText, parsingOptions) |> Async.RunSynchronously

                     Some(parseResults.ParseTree, sourceText)
                 with
                 | :? IOException
                 | :? UnauthorizedAccessException -> None)
         ))

        // every sweepable source already swept by an earlier compilation of
        // this run (same defines, or directive-free): the sweep will visit
        // zero files, so the project typecheck buys nothing. Only while the
        // tree is untouched (or dry) — applied fixes make the recheck the
        // cross-project verification, which must stay
        let skipCompilationCheck =
            (opts.DryRun || runTotalApplied = 0)
            && onlyFile.IsNone
            // the api pass ignores the sweep dedup and can still WRITE this
            // project's files — skipping would leave it an empty snapshot to
            // roll back to and a zero error baseline to verify against
            && not opts.ApiChanges
            && (let sweepable =
                    options.SourceFiles |> Array.filter (Configuration.isIgnoredPath >> not)

                sweepable.Length > 0
                && sweepable
                   |> Array.forall (fun f ->
                       sweptFiles.Contains(Path.GetFullPath(f).ToLowerInvariant(), fileSweepKey (definesKey options) f)))

        let snapshot =
            if opts.DryRun || skipCompilationCheck then
                Map.empty
            else
                takeSnapshot options.SourceFiles

        // fixes a verification rollback rejected; never re-applied this run
        let suppressed =
            System.Collections.Generic.HashSet<string * string * string * string>()

        let baselineErrors =
            if skipCompilationCheck then
                printfn "  (every source file already swept in an earlier compilation — project check skipped)"
                0
            else
                printf "typechecking the project... "
                Console.Out.Flush()
                let baselineSw = Stopwatch.StartNew()
                let n = errorCount checker options
                baselineSw.Stop()
                printfn $"{baselineSw.ElapsedMilliseconds} ms"
                n

        // a SCRIPT that does not resolve is not refused: scripts routinely
        // reference things fsi would supply at run time (or nothing at all —
        // a README-snippet checker), and the syntactic rules still apply.
        // Projects keep the refusal: they are supposed to compile
        let degradedScript =
            baselineErrors > 0
            && not opts.ParseOnly
            && (match target with
                | Target.Script _ -> true
                | Target.Project _ -> false)

        let analyzers =
            if degradedScript then
                analyzers |> List.filter (analyzerName >> parseOnlySafeAnalyzers.Contains)
            else
                analyzers

        if (opts.ParseOnly || degradedScript) && baselineErrors > 0 then
            // expected: nothing was resolved. The count still serves as the
            // end-of-run regression baseline — a fix that breaks the parse
            // RAISES it and is put back
            printfn
                $"  ({baselineErrors} unresolved-reference error(s) ignored; syntactic rules only ({analyzers.Length}))"

        if baselineErrors > 0 && not opts.ParseOnly && not degradedScript then
            eprintfn $"The project has {baselineErrors} error(s) before any fix; fix those first:"

            if showHeader && not opts.DryRun then
                // in a multi-compilation run these "pre-existing" errors can
                // be an earlier project's applied fixes breaking a shared
                // source file — that is our doing, not the caller's. A dry
                // run modifies nothing, so there the errors are simply
                // pre-existing
                eprintfn
                    "  (multi-project run: if an earlier project was just modified, its fixes may have introduced these — review the diff)"

            for d in projectErrors checker options |> Array.truncate 5 do
                eprintfn $"  {d.FileName}({d.StartLine},{d.StartColumn}): {d.Message}"

            1
        else
            // asking for one file and getting edits in its callers would be
            // a surprise, and that is exactly what the cross-file rules do
            if opts.ApiChanges && onlyFile.IsSome then
                printfn "  (api pass skipped: a single file was named, and these fixes edit call sites elsewhere)"

            // Everything the run actually wrote, across the api-changes
            // rounds and the normal passes. Zero means an untouched tree:
            // there is nothing to arbitrate, so the end-of-run error
            // recount and the all-frameworks verification build are pure
            // cost and are skipped — on a large solution that is one full
            // `dotnet build` per framework pass of every clean project.
            let mutable totalApplied = 0

            if opts.ApiChanges && onlyFile.IsNone then
                // iterated: a suggestion held back because its edits
                // nest inside another suggestion's applies next round
                let mutable apiPass = 0
                let mutable apiApplied = -1

                while apiPass < opts.MaxPasses && apiApplied <> 0 do
                    apiPass <- apiPass + 1
                    ProjectSources.invalidate ()
                    printfn $"api pass {apiPass}:"

                    let applied, changedFiles =
                        runApiPass checker options opts.Codes opts.DryRun suppressed

                    apiApplied <- applied
                    totalApplied <- totalApplied + applied
                    runTotalApplied <- runTotalApplied + applied
                    printfn $"  {apiApplied} api-changing fix(es) applied"

                    if opts.DryRun then
                        // nothing was written, so a second round would
                        // only repeat the same report
                        apiApplied <- 0
                    elif apiApplied > 0 then
                        // a cross-file suggestion's edits all land in the
                        // same pass, so a rollback keeps them consistent
                        verifyPass checker options baselineErrors suppressed changedFiles |> ignore

            let mutable pass = 0
            let mutable lastApplied = -1

            while pass < opts.MaxPasses && lastApplied <> 0 do
                pass <- pass + 1
                ProjectSources.invalidate ()
                printfn $"pass {pass}:"

                let applied, changedFiles =
                    runPass
                        checker
                        options
                        analyzers
                        opts.Codes
                        opts.DryRun
                        opts.ApiChanges
                        opts.Jobs
                        onlyFile
                        suppressed

                lastApplied <- applied
                totalApplied <- totalApplied + applied
                runTotalApplied <- runTotalApplied + applied

                let prefix = if opts.DryRun then "would be " else ""
                printfn $"  {lastApplied} fix(es) {prefix}applied"

                if opts.DryRun then
                    lastApplied <- 0 // a dry run never converges; stop after one pass
                elif lastApplied > 0 then
                    // verify while the pre-pass texts are in hand; a clean
                    // result warms the next pass's project check, so this
                    // REPLACES the end-of-run check rather than adding one
                    verifyPass checker options baselineErrors suppressed changedFiles |> ignore

            if not opts.DryRun && lastApplied > 0 && pass = opts.MaxPasses then
                eprintfn
                    $"did not converge: fixes were still being applied after {opts.MaxPasses} pass(es) — rerun to continue, or raise --max-passes"

            if opts.DryRun then
                markSwept options
                0
            elif totalApplied = 0 then
                // no file was written: the tree is exactly as the baseline
                // check found it, so re-counting errors and rebuilding every
                // framework would verify nothing
                markSwept options
                0
            else
                checker.InvalidateConfiguration options
                let finalErrors = errorCount checker options

                if finalErrors > baselineErrors then
                    // per-pass verification should have made this
                    // unreachable; if something slipped through anyway, do
                    // not leave a broken tree behind
                    let restored = restoreSnapshot snapshot

                    eprintfn
                        $"Applying introduced {finalErrors - baselineErrors} error(s); the {restored} changed file(s) were put back."

                    1
                // The check above only covers the framework we analysed. A
                // multi-targeted project has others, and a fix valid for one
                // can fail on another, so build the lot before claiming
                // success.
                elif isMultiTargeted target && not opts.ParseOnly then
                    match target with
                    | Target.Project(project, _) ->
                        printfn "verifying every target framework..."

                        match buildAllFrameworks project with
                        | Ok() ->
                            printfn "done; every target framework still builds"
                            markSwept options
                            0
                        | Error output ->
                            // This pass changed code the other frameworks
                            // also compile — shared code, outside any #if —
                            // and offered something only this framework can
                            // resolve. Reporting is not enough: put the
                            // files back, or the caller is left with a
                            // project that does not build.
                            //
                            // ...unless the framework was ALREADY broken by
                            // something this run never wrote (a vendored
                            // paket-files source, most famously): test by
                            // un-applying — if the build still fails, the
                            // breakage is not ours and the fixes go back.
                            let currentTexts =
                                snapshot
                                |> Map.toList
                                |> List.choose (fun (path, _) ->
                                    try
                                        Some(path, File.ReadAllText path)
                                    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                                        None)

                            let restored = restoreSnapshot snapshot

                            match buildAllFrameworks project with
                            | Error _ ->
                                for path, text in currentTexts do
                                    writeSource path text

                                eprintfn
                                    "A target framework fails to build, but it fails WITHOUT this run's fixes too — pre-existing breakage, fixes kept:"

                                eprintfn $"{output}"
                                1
                            | Ok() ->
                                eprintfn
                                    $"Applying broke a target framework this run did not analyze, so the {restored} file(s) it changed were put back:"

                                eprintfn $"{output}"
                                1
                    | Target.Script _ ->
                        printfn "done; project still checks clean"
                        markSwept options
                        0
                else
                    printfn "done; project still checks clean"
                    markSwept options
                    0

/// The fingerprint set of an earlier SARIF report, for --baseline.
let private loadBaseline (path: string) : Result<Set<string>, string> =
    try
        use doc = JsonDocument.Parse(File.ReadAllText path)

        let prints =
            [ for run in doc.RootElement.GetProperty("runs").EnumerateArray() do
                  match run.TryGetProperty "results" with
                  | true, results ->
                      for result in results.EnumerateArray() do
                          match result.TryGetProperty "partialFingerprints" with
                          | true, fps ->
                              match fps.TryGetProperty FingerprintKey with
                              | true, v -> v.GetString()
                              | _ -> ()
                          | _ -> ()
                  | _ -> () ]

        Ok(Set.ofList prints)
    with ex ->
        Error $"could not read baseline '{path}': {ex.Message}"

let private severityName (s: Severity) =
    match s with
    | Severity.Error -> "error"
    | Severity.Warning -> "warning"
    | Severity.Info -> "info"
    | Severity.Hint -> "hint"

let private findingsPayload (findings: ReportedFinding list) =
    [ for f in findings ->
          dict
              [ "code", box f.Code
                "severity", box (severityName f.Severity)
                "fixable", box f.Fixable
                "file", box f.File
                "startLine", box f.StartLine
                "startColumn", box f.StartColumn
                "endLine", box f.EndLine
                "endColumn", box f.EndColumn
                "message", box f.Message
                "fingerprint", box f.Fingerprint
                "snippet", box f.Snippet ] ]

/// The run's findings as one JSON document (--format json and --mcp).
let private findingsAsJson (findings: ReportedFinding list) (baselined: int) =
    let payload =
        dict
            [ "findings", box (findingsPayload findings)
              "baselineSuppressed", box baselined
              "commentSuppressed", box commentSuppressed
              "suppressionsOverridden", box suppressionOverridden ]

    JsonSerializer.Serialize(payload, JsonSerializerOptions(WriteIndented = true))

let private rulesAsRows () =
    RuleCatalog.allRules
    |> List.map (fun (code, category) -> code, RuleCatalog.name category, Configuration.isEnabledIn Map.empty code "")

/// Order a multi-compilation run NARROWEST TARGET FIRST, solution-wide.
/// Within one project the narrowest framework already goes first; across
/// projects the same principle protects shared source files — a fix
/// proposed while analysing the net8.0-only project would compile there
/// and break the netstandard2.0 sibling a whole compilation later. With
/// the restrictive context up front, capability-gated rules see the
/// narrow surface, the fixes they offer hold everywhere wider, and the
/// sweep dedup then skips the already-clean shared files. Scripts keep
/// their place at the end; projects whose framework a textual read
/// cannot see (Directory.Build.props inheritance) sort after the known
/// ones, in their original order.
let private orderNarrowestFirst (targets: Target list) =
    if targets.Length < 2 then
        targets
    else
        let projectRank (path: string) =
            try
                let text = File.ReadAllText path

                let m =
                    Text.RegularExpressions.Regex.Match(text, "<TargetFrameworks?>([^<]+)</TargetFrameworks?>")

                if m.Success then
                    m.Groups.[1].Value.Split ';'
                    |> Array.map _.Trim()
                    |> Array.filter (fun t -> t <> "" && not (t.Contains "$("))
                    |> Array.map tfmRank
                    |> Array.sort
                    |> Array.tryHead
                    |> Option.defaultValue (98, 0)
                else
                    (98, 0)
            with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                (98, 0)

        targets
        |> List.sortBy (fun t ->
            match t with
            | Target.Project(path, _) -> projectRank path
            | Target.Script _ -> (99, 0))

/// The whole run for one Options value: resolve targets, sweep, verify,
/// report. The checker comes from the caller so a resident host (--mcp)
/// can keep it — and every reference assembly FCS has parsed — warm
/// between calls.
let private executeRun (checker: FSharpChecker) (opts: Options) : int =
    match resolveTargets opts.Target with
    | Error message ->
        eprintfn $"{message}"
        2
    | Ok targets ->
        let targets = orderNarrowestFirst targets
        let several = targets.Length > 1

        if several then
            printfn $"{targets.Length} compilations to work through (narrowest target first)"

        sweptFiles.Clear()
        directiveFreeCache.Clear()
        runTotalApplied <- 0
        honorAllSuppressions <- opts.HonorSuppressions
        parseOnlyRun <- opts.ParseOnly
        showNotes <- opts.Notes
        lock heldNoteCounts heldNoteCounts.Clear
        // the corpus harness runs main in-process, so per-run stores
        // must not leak findings across invocations
        lock reportedFindings (fun () ->
            reportedFindings.Clear()
            reportedKeys.Clear())

        printedNotes.Clear()

        // A multi-targeted project is really several compilations: each
        // framework activates its own #if branches, and code behind
        // another framework's is not in the parse tree at all. So work
        // through them rather than making the caller name each one —
        // narrowest first, so the fixes valid everywhere land before any
        // that only suit a wider surface. The final all-framework build
        // is what catches a fix that does not generalise.
        let runOne target =
            match opts.Framework, frameworksOf target with
            // parse-only has no per-framework defines to vary; one pass
            | _ when opts.ParseOnly -> runTarget checker opts several target
            | "", (_ :: _ :: _ as frameworks) ->
                let conditionals =
                    match target with
                    | Target.Project(project, _) -> sourcesUseConditionals project
                    | Target.Script _ -> true

                if not conditionals then
                    // no #if anywhere: every framework parses the same
                    // tree, so one sweep covers them all — and the
                    // final all-frameworks build still verifies the rest
                    printfn
                        $"{Path.GetFileName opts.Target}: {frameworks.Length} target frameworks, no conditional compilation — sweeping the narrowest only"

                    runTarget
                        checker
                        { opts with
                            Framework = List.head frameworks }
                        true
                        target
                else
                    printfn $"{Path.GetFileName opts.Target}: {frameworks.Length} target frameworks"

                    // legacy targets present AND the fsproj defines its
                    // own modern-only constant: capability rules
                    // (FR0038/FR0106) emit #if <that constant> pairs on
                    // the modern passes instead of fixes the legacy
                    // half cannot compile. No such constant, no dual
                    // emission — nothing is invented.
                    let isLegacy (tfm: string) =
                        tfm.StartsWith "net4"
                        || tfm.StartsWith "netstandard1"
                        || tfm = "netstandard2.0"
                        || tfm = "netcoreapp2.0"

                    let legacyTfms, modernTfms = frameworks |> List.partition isLegacy

                    let dualConstant =
                        if opts.NoIfDefs || legacyTfms.IsEmpty then
                            None
                        else
                            match target with
                            | Target.Project(project, _) -> chooseDualConstant project modernTfms legacyTfms
                            | Target.Script _ -> None

                    match dualConstant with
                    | Some constant ->
                        printfn
                            $"  (capability fixes will pair with the project's own #if {constant} for the legacy frameworks)"
                    | None -> ()

                    let results =
                        frameworks
                        |> List.map (fun tfm ->
                            Environment.SetEnvironmentVariable(
                                "FSREF_DUAL_TFM",
                                (match dualConstant with
                                 | Some c when not (isLegacy tfm) -> c
                                 | _ -> null)
                            )

                            runTarget checker { opts with Framework = tfm } true target)

                    Environment.SetEnvironmentVariable("FSREF_DUAL_TFM", null)
                    results |> List.fold max 0
            | _ -> runTarget checker opts several target

        try
            let exitCode = targets |> List.map runOne |> List.fold max 0

            match opts.Report with
            | Some reportPath ->
                writeSarifReport reportPath (lock reportedFindings (fun () -> List.ofSeq reportedFindings))
                printfn $"{reportedFindings.Count} finding(s) written to {reportPath}"
            | None -> ()

            let heldNotes = lock heldNoteCounts (fun () -> heldNoteCounts |> List.ofSeq)

            if not heldNotes.IsEmpty then
                let total = heldNotes |> List.sumBy (fun kv -> kv.Value)

                let breakdown =
                    heldNotes
                    |> List.sortByDescending (fun kv -> kv.Value)
                    |> List.map (fun kv -> $"{kv.Value} {kv.Key}")
                    |> String.concat ", "

                printfn $"  {total} advisory note(s) held: {breakdown} — list with --notes, export with --report"

            if baselineSuppressed > 0 then
                printfn $"  ({baselineSuppressed} finding(s) matched the baseline and were suppressed)"

            if commentSuppressed > 0 then
                printfn $"  ({commentSuppressed} finding(s) silenced by suppression comments)"

            if suppressionOverridden > 0 then
                printfn
                    $"  ({suppressionOverridden} suppression comment(s) not honored by the \"suppressions\" policy — reported above, never auto-fixed)"

            if exitCode = 0 && opts.FailOnFindings && reportedFindings.Count > 0 then
                3
            else
                exitCode
        finally
            // runTarget sets these for the rule variants; the corpus
            // harness runs main IN-PROCESS, so a leaked flag would make
            // later analyzer calls in the same process api-changes- or
            // forced-code-scoped
            Environment.SetEnvironmentVariable("FSREF_API_CHANGES", null)
            Environment.SetEnvironmentVariable("FSREF_FORCE_CODES", null)
            Environment.SetEnvironmentVariable("FSREF_DUAL_TFM", null)

/// --rules: the catalog, human table by default, JSON on request.
let private printRules (json: bool) =
    if json then
        let payload =
            [ for code, category, enabledByDefault in rulesAsRows () ->
                  dict
                      [ "code", box code
                        "category", box category
                        "enabledByDefault", box enabledByDefault ] ]

        printfn $"{JsonSerializer.Serialize(payload, JsonSerializerOptions(WriteIndented = true))}"
    else
        for code, category, enabledByDefault in rulesAsRows () do
            let marker = if enabledByDefault then "" else "  (off by default)"
            printfn $"%s{code}  %-12s{category}%s{marker}"

/// One MCP tool-call response body: text content plus the protocol wrapper.
let private mcpToolResult (text: string) =
    dict [ "content", box [ dict [ "type", box "text"; "text", box text ] ] ]

[<return: Struct>]
let inline private (|IsNullOrWhiteSpace|_|) (input: string) =
    if String.IsNullOrWhiteSpace input then
        ValueSome input
    else
        ValueNone

/// --mcp: a minimal MCP server over stdio — newline-delimited JSON-RPC
/// 2.0, no extra dependencies, and one warm FSharpChecker across every
/// call, which is the entire point: the first analyze pays the reference
/// parse, the rest answer from a hot cache. Progress prose is diverted to
/// stderr so the protocol stream stays clean.
let private runMcp () =
    let protocolOut = Console.Out
    Console.SetOut Console.Error

    let checker = FSharpChecker.Create(keepAssemblyContents = true)

    let respond (idJson: string) (resultJson: string) =
        protocolOut.WriteLine($"{{\"jsonrpc\":\"2.0\",\"id\":{idJson},\"result\":{resultJson}}}")
        protocolOut.Flush()

    let respondError (idJson: string) (code: int) (message: string) =
        let msg = JsonSerializer.Serialize message

        protocolOut.WriteLine(
            $"{{\"jsonrpc\":\"2.0\",\"id\":{idJson},\"error\":{{\"code\":{code},\"message\":{msg}}}}}"
        )

        protocolOut.Flush()

    let serialize (o: obj) = JsonSerializer.Serialize o

    let toolsJson =
        serialize (
            dict
                [ "tools",
                  box
                      [ dict
                            [ "name", box "analyze"
                              "description",
                              box
                                  "Analyze an F# project, script or directory with fsharp-refactor. Dry-run by default: reports findings without editing. Set apply=true to write the fixes (build-verified). Returns findings as JSON with stable fingerprints and source snippets."
                              "inputSchema",
                              box (
                                  dict
                                      [ "type", box "object"
                                        "properties",
                                        box (
                                            dict
                                                [ "target",
                                                  box (
                                                      dict
                                                          [ "type", box "string"
                                                            "description",
                                                            box "fsproj, fsx, sln, directory or glob to analyze" ]
                                                  )
                                                  "codes",
                                                  box (
                                                      dict
                                                          [ "type", box "string"
                                                            "description",
                                                            box "comma-separated rule codes to restrict to" ]
                                                  )
                                                  "categories",
                                                  box (
                                                      dict
                                                          [ "type", box "string"
                                                            "description",
                                                            box
                                                                "comma-separated: correctness,performance,idiom,cosmetic" ]
                                                  )
                                                  "parseOnly",
                                                  box (
                                                      dict
                                                          [ "type", box "boolean"
                                                            "description", box "no MSBuild, syntactic rules only" ]
                                                  )
                                                  "apply",
                                                  box (
                                                      dict
                                                          [ "type", box "boolean"
                                                            "description", box "write the fixes (default: dry-run)" ]
                                                  ) ]
                                        )
                                        "required", box [ "target" ] ]
                              ) ]
                        dict
                            [ "name", box "list_rules"
                              "description", box "The rule catalog: code, category, enabled-by-default."
                              "inputSchema", box (dict [ "type", box "object"; "properties", box (dict []) ]) ] ] ]
        )

    let handleAnalyze (args: JsonElement) =
        let getString name =
            match args.TryGetProperty(name: string) with
            | true, v when v.ValueKind = JsonValueKind.String -> Some(v.GetString())
            | _ -> None

        let getBool name =
            match args.TryGetProperty(name: string) with
            | true, v -> v.ValueKind = JsonValueKind.True
            | _ -> false

        match getString "target" with
        | None -> Error "analyze needs a 'target'"
        | Some target ->

            match parseArgs [| target |] with
            | Error message -> Error message
            | Ok baseOpts ->

                let codes =
                    getString "codes"
                    |> Option.map (fun s ->
                        s.Split ',' |> Array.map (fun c -> c.Trim().ToUpperInvariant()) |> Set.ofArray)

                let categories =
                    getString "categories"
                    |> Option.map (fun s -> s.Split ',' |> Array.choose RuleCatalog.parse |> Set.ofArray)

                let opts =
                    { baseOpts with
                        DryRun = not (getBool "apply")
                        ParseOnly = getBool "parseOnly"
                        Codes = codes
                        ExplicitCodes = codes
                        Categories = categories }
                    |> applyCategories

                lock reportedFindings (fun () ->
                    reportedFindings.Clear()
                    reportedKeys.Clear()
                    baselineSuppressed <- 0
                    commentSuppressed <- 0
                    suppressionOverridden <- 0)

                printedNotes.Clear()
                let exitCode = executeRun checker opts
                let findings = lock reportedFindings (fun () -> List.ofSeq reportedFindings)

                let body =
                    dict
                        [ "exitCode", box exitCode
                          "applied", box (not opts.DryRun)
                          "findingCount", box findings.Length
                          "findings", box (findingsPayload findings)
                          "baselineSuppressed", box baselineSuppressed
                          "commentSuppressed", box commentSuppressed
                          "suppressionsOverridden", box suppressionOverridden ]

                Ok(JsonSerializer.Serialize body)

    let mutable running = true

    while running do
        match Console.In.ReadLine() with
        | null -> running <- false
        | IsNullOrWhiteSpace line -> ()
        | line ->
            let idJson, method_, params_ =
                try
                    use doc = JsonDocument.Parse line
                    let root = doc.RootElement

                    let id =
                        match root.TryGetProperty "id" with
                        | true, v -> v.GetRawText()
                        | _ -> "null"

                    let m =
                        match root.TryGetProperty "method" with
                        | true, v -> v.GetString()
                        | _ -> ""

                    let p =
                        match root.TryGetProperty "params" with
                        | true, v -> Some(v.Clone())
                        | _ -> None

                    id, m, p
                with _ ->
                    "null", "", None

            match method_ with
            | "initialize" ->
                respond
                    idJson
                    """{"protocolVersion":"2024-11-05","capabilities":{"tools":{}},"serverInfo":{"name":"fsharp-refactor","version":"0.6.6"}}"""
            | "notifications/initialized"
            | "notifications/cancelled" -> ()
            | "ping" -> respond idJson "{}"
            | "tools/list" -> respond idJson toolsJson
            | "tools/call" ->
                let name, args =
                    match params_ with
                    | Some p ->
                        let n =
                            match p.TryGetProperty "name" with
                            | true, v -> v.GetString()
                            | _ -> ""

                        let a =
                            match p.TryGetProperty "arguments" with
                            | true, v -> v
                            | _ -> JsonDocument.Parse("{}").RootElement

                        n, a
                    | None -> "", JsonDocument.Parse("{}").RootElement

                match name with
                | "list_rules" ->
                    let rules =
                        [ for code, category, enabledByDefault in rulesAsRows () ->
                              dict
                                  [ "code", box code
                                    "category", box category
                                    "enabledByDefault", box enabledByDefault ] ]

                    respond idJson (serialize (mcpToolResult (serialize rules)))
                | "analyze" ->
                    try
                        (handleAnalyze args)
                        |> Result.map (mcpToolResult >> serialize >> respond idJson)
                        |> Result.defaultWith (fun msg -> respondError idJson -32602 msg)
                    with ex ->
                        respondError idJson -32603 $"analyze failed: {ex.Message}"
                | other -> respondError idJson -32601 $"unknown tool '{other}'"
            | "" -> respondError idJson -32700 "unparseable request"
            | notification when not (notification.StartsWith "notifications/") && idJson <> "null" ->
                respondError idJson -32601 $"unknown method '{notification}'"
            | _ -> ()

    0

[<EntryPoint>]
let main argv =
    match parseArgs argv with
    | Error message ->
        eprintfn $"{message}"
        2
    | Ok opts when opts.ShowHelp ->
        printfn $"{helpText}"
        0
    | Ok opts when opts.ListRules ->
        printRules opts.Json
        0
    | Ok opts when opts.Mcp -> runMcp ()
    // no arguments at all is a question, not a mistake: show the help
    | Ok opts when opts.Target = "" ->
        printfn $"{helpText}"
        2
    | Ok opts ->
        let baseline =
            match opts.Baseline with
            | Some path -> loadBaseline path
            | None -> Ok Set.empty

        match baseline with
        | Error message ->
            eprintfn $"{message}"
            2
        | Ok prints ->
            baselineFingerprints <- prints
            baselineSuppressed <- 0
            commentSuppressed <- 0
            suppressionOverridden <- 0

            // --format json: prose to stderr, one clean JSON document on
            // the real stdout. The default output stays human-readable.
            let realOut = Console.Out

            if opts.Json then
                Console.SetOut Console.Error

            // ONE checker for the whole run: FCS caches parsed reference
            // assemblies on the instance, and a twenty-project solution's
            // flavors share nearly all of them — a fresh checker per
            // compilation was paying that parse twenty times over.
            // (Analyzers may read the typed tree, hence assembly contents.)
            let checker = FSharpChecker.Create(keepAssemblyContents = true)
            let code = executeRun checker opts

            if opts.Json then
                Console.SetOut realOut
                let findings = lock reportedFindings (fun () -> List.ofSeq reportedFindings)
                printfn $"{findingsAsJson findings baselineSuppressed}"

            code
