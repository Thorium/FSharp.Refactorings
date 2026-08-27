/// Applies FSharp.Refactorings quick fixes from the command line.
///
///     dotnet tool install --global fsharp-refactorings-apply
///     fsharp-refactor Your.fsproj
///
/// (from this repository:
/// `dotnet run --project src/FSharp.Refactorings.Apply -- Your.fsproj`)
///
/// The stock `fsharp-analyzers` CLI only REPORTS: fixes reach editors and
/// SARIF, never the files. This tool closes that gap:
///
///   1. `dotnet msbuild --getItem:FscCommandLineArgs` yields the exact
///      compiler arguments (no project-cracking library needed)
///   2. every [<CliAnalyzer>] in FSharp.Refactorings.Analyzers runs against
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
///     --dry-run                report what would be applied, change nothing
///     --api-changes            also apply CROSS-FILE fixes (rules that
///                              rewrite call sites of internal/public
///                              symbols across the project); without it
///                              those fixes are held back and counted
///     --jobs <n>               files typechecked at once (default 4, capped
///                              by the core count). Trades CPU for wall
///                              clock: FCS reuses each file's prefix within
///                              one incremental build, and parallel checks
///                              give that reuse up, so the gain peaks around
///                              4 and reverses if pushed higher. --jobs 1
///                              restores the sequential sweep
///     --max-passes <n>         fix-then-reanalyze iterations (default 5)
module FSharp.Refactorings.Apply.Program

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text.Json
open FSharp.Analyzers.SDK
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharp.Refactorings

type private Options =
    { Target: string
      Codes: Set<string> option
      DryRun: bool
      ApiChanges: bool
      MaxPasses: int
      Jobs: int }

[<TailCall>]
let rec private parseArgsLoop opts args =
    match args with
    | [] -> Ok opts
    // --project and --script still work, but the kind is inferred from the
    // extension either way, so a bare path is enough
    | "--project" :: path :: rest
    | "--script" :: path :: rest
    | "--target" :: path :: rest -> parseArgsLoop { opts with Target = path } rest
    | "--codes" :: codes :: rest ->
        parseArgsLoop
            { opts with
                Codes = Some(codes.Split(',') |> Array.map _.Trim() |> Set.ofArray) }
            rest
    | "--dry-run" :: rest -> parseArgsLoop { opts with DryRun = true } rest
    | "--api-changes" :: rest -> parseArgsLoop { opts with ApiChanges = true } rest
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

let private parseArgs (argv: string[]) =
    parseArgsLoop
        { Target = ""
          Codes = None
          DryRun = false
          ApiChanges = false
          MaxPasses = 5
          // Measured sweet spot. FCS reuses each file's prefix within one
          // incremental build, so parallel checks buy wall clock by giving
          // that reuse up: on a 113-file project the sweep runs 70 s at one
          // job, 53 s at four, and back up to 61 s at eleven. Clamped to
          // 2..4 — a small machine is not oversubscribed, and even a
          // single-core one still overlaps a check with an analyzer pass.
          Jobs = min 4 (max 2 Environment.ProcessorCount) }
        (List.ofArray argv)

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
                let psi =
                    ProcessStartInfo(
                        FileName = vswhere,
                        Arguments = "-latest -requires Microsoft.Component.MSBuild -find MSBuild\\**\\Bin\\MSBuild.exe",
                        RedirectStandardOutput = true,
                        UseShellExecute = false
                    )

                use p = Process.Start psi
                let out = p.StandardOutput.ReadToEnd()
                p.WaitForExit()

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

/// The project's fsc arguments, straight from MSBuild. SDK-style projects
/// go through `dotnet`; old-style (net48-era) projects need Visual
/// Studio's MSBuild, whose imports do not evaluate under the SDK's.
let private fscArgs (projectPath: string) =
    let projectText =
        try
            File.ReadAllText projectPath
        with
        | :? IOException
        | :? UnauthorizedAccessException -> ""

    let isSdkStyle = projectText.Contains "Sdk="

    // A multi-targeted project builds "outer" and dispatches one inner build
    // per framework. CoreCompile — and so FscCommandLineArgs — only runs in
    // the inner ones, so the outer query comes back empty. Pin a framework
    // to get an inner build.
    //
    // The FIRST one listed, because that is what a project usually leads
    // with and it is typically the most restrictive; analysing against the
    // narrowest target keeps a fix valid for the others, where the reverse
    // could offer an API the older targets do not have.
    let targetFramework =
        let m =
            Text.RegularExpressions.Regex.Match(projectText, "<TargetFrameworks>([^<]+)</TargetFrameworks>")

        if m.Success then
            m.Groups.[1].Value.Split(';')
            |> Array.map _.Trim()
            |> Array.filter (fun tfm -> tfm <> "")
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

        let psi =
            ProcessStartInfo(
                FileName = runner,
                Arguments = finalArgs,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            )

        use p = Process.Start psi
        let stdout = p.StandardOutput.ReadToEnd()
        let stderr = p.StandardError.ReadToEnd()
        p.WaitForExit()
        p.ExitCode, stdout, stderr

    // a REAL build first: project references must exist on disk for the
    // args-only pass below (SkipCompilerExecution skips them too), and a
    // project that does not build has no business being rewritten
    let buildExit, buildOut, buildErr = run $"build \"{projectPath}\"{tfmArg}"

    if buildExit <> 0 then
        Error $"dotnet build failed — fix the build before applying fixes:\n{buildOut}\n{buildErr}"
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
    let assembly = typeof<FSharp.Refactorings.RedundantParens.Suggestion>.Assembly

    [ for t in assembly.GetTypes() do
          for m in t.GetMethods(BindingFlags.Static ||| BindingFlags.Public) do
              if m.GetCustomAttributes(typeof<CliAnalyzerAttribute>, false).Length > 0 then
                  m ]

let private projectErrors (checker: FSharpChecker) (options: FSharpProjectOptions) =
    let results = checker.ParseAndCheckProject options |> Async.RunSynchronously

    results.Diagnostics
    |> Array.filter (fun d -> d.Severity = FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error)

let private errorCount (checker: FSharpChecker) (options: FSharpProjectOptions) = (projectErrors checker options).Length

/// Apply grouped edits, bottom-up per file, skipping any fix overlapping
/// one already taken; the original text is verified before each splice.
/// Returns the number of fixes applied.
let private applyEditGroups
    (dryRun: bool)
    (editsByFile: System.Collections.Generic.Dictionary<string, ResizeArray<string * Fix>>)
    =
    let mutable applied = 0

    for kv in editsByFile do
        let file = kv.Key
        let text = File.ReadAllText file

        let edits =
            kv.Value
            |> Seq.sortByDescending (fun (_, f) -> f.FromRange.StartLine, f.FromRange.StartColumn)
            |> List.ofSeq

        let mutable current = text
        let mutable appliedRanges: Range list = []

        let overlaps (r: Range) =
            appliedRanges
            |> List.exists (fun a ->
                Range.rangeContainsPos a r.Start
                || Range.rangeContainsPos a r.End
                || Range.rangeContainsRange r a)

        for code, f in edits do
            if not (overlaps f.FromRange) then
                let lines = current.Split('\n')

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
                    current <- current.Remove(startIndex, endIndex - startIndex).Insert(startIndex, f.ToText)
                    appliedRanges <- f.FromRange :: appliedRanges
                    applied <- applied + 1
                    printfn $"  {code} {Path.GetFileName file}({f.FromRange.StartLine},{f.FromRange.StartColumn})"

        if current <> text && not dryRun then
            File.WriteAllText(file, current)

    applied

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
    =
    if options.SourceFiles |> Array.exists (fun f -> f.EndsWith ".fsi") then
        printfn "  (api pass skipped: signature files would need the same changes)"
        0
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
            System.Collections.Generic.Dictionary<string, ResizeArray<string * Fix>>(StringComparer.OrdinalIgnoreCase)

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
                printfn $"  {s.Code} {s.FunctionName}: held back this round (edits nest inside another change)"
            else
                printfn $"  {s.Code} {s.FunctionName}: {s.Edits.Length} edit(s) across the project"

                for range, original, replacement in s.Edits do
                    let target = Path.GetFullPath range.FileName

                    let fix =
                        { FromRange = range
                          FromText = original
                          ToText = replacement }

                    match editsByFile.TryGetValue target with
                    | true, existing -> existing.Add(s.Code, fix)
                    | false, _ ->
                        let fresh = ResizeArray()
                        fresh.Add(s.Code, fix)
                        editsByFile.[target] <- fresh

                    match acceptedRanges.TryGetValue target with
                    | true, ranges -> ranges.Add range
                    | false, _ ->
                        let fresh = ResizeArray()
                        fresh.Add range
                        acceptedRanges.[target] <- fresh

        applyEditGroups dryRun editsByFile

/// One analyze-and-apply pass over every file. Returns the number of fixes
/// applied.
let private runPass
    (checker: FSharpChecker)
    (options: FSharpProjectOptions)
    (analyzers: MethodInfo list)
    (codes: Set<string> option)
    (dryRun: bool)
    (apiChanges: bool)
    (jobs: int)
    (onlyFile: string option)
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
        System.Collections.Generic.Dictionary<string, ResizeArray<string * Fix>>(StringComparer.OrdinalIgnoreCase)

    let mutable crossFileSkipped = 0

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
                      AnalyzerIgnoreRanges = Map.empty }

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

                let messages =
                    collected
                    |> Seq.filter (fun msg ->
                        not msg.Fixes.IsEmpty
                        && codes |> Option.forall (fun wanted -> wanted.Contains msg.Code))
                    |> List.ofSeq

                return
                    {| File = file
                       CheckMs = checkSw.ElapsedMilliseconds
                       Timings = List.ofSeq timings
                       HasErrors = OptionModule.hasErrors checkResults
                       Messages = messages |}
            | FSharpCheckFileAnswer.Aborted ->
                return
                    {| File = file
                       CheckMs = checkSw.ElapsedMilliseconds
                       Timings = []
                       HasErrors = true
                       Messages = [] |}
        }

    let sweepSw = Stopwatch.StartNew()

    // naming one source file means analyzing its project — the references
    // and the files before it are what give its names meaning — but
    // sweeping only that file
    let filesToSweep =
        match onlyFile with
        | Some only ->
            options.SourceFiles
            |> Array.filter (fun f -> String.Equals(Path.GetFullPath f, only, StringComparison.OrdinalIgnoreCase))
        | None -> options.SourceFiles

    let outcomes =
        filesToSweep
        |> Array.map analyzeFile
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

        for msg in outcome.Messages do
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

                if sameFile || apiChanges then
                    match editsByFile.TryGetValue target with
                    | true, existing -> existing.Add(msg.Code, f)
                    | false, _ ->
                        let fresh = ResizeArray()
                        fresh.Add(msg.Code, f)
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

    applyEditGroups dryRun editsByFile

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

    // Widen from the file outwards. Each level searches beneath itself too,
    // because a project commonly compiles sources from a sibling folder
    // (`<Compile Include="..\Code\Thing.fs" />`), so walking up through
    // parent directories alone would miss it. Bounded, to avoid ending up
    // scanning a whole drive.
    let mutable dir = Path.GetDirectoryName full
    let mutable found = None
    let mutable levels = 0

    while found.IsNone && not (String.IsNullOrEmpty dir) && levels < 6 do
        found <-
            Directory.EnumerateFiles(dir, "*.fsproj", SearchOption.AllDirectories)
            |> Seq.filter (fun p -> not (p.Contains @"\obj\" || p.Contains @"\bin\"))
            |> Seq.tryFind lists

        dir <- Path.GetDirectoryName dir
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
            Directory.EnumerateFiles(root, leaf, SearchOption.AllDirectories)
            |> Seq.filter (fun f -> not (f.Contains @"\obj\" || f.Contains @"\bin\"))
            |> List.ofSeq
        else
            []

    let fromDirectory (dir: string) =
        let solutions =
            [ yield! Directory.EnumerateFiles(dir, "*.slnx")
              yield! Directory.EnumerateFiles(dir, "*.sln") ]

        match solutions with
        | solution :: _ -> projectsInSolution solution |> List.map (fun p -> Target.Project(p, None))
        | [] ->
            Directory.EnumerateFiles(dir, "*.fsproj", SearchOption.AllDirectories)
            |> Seq.filter (fun f -> not (f.Contains @"\obj\" || f.Contains @"\bin\"))
            |> Seq.map (fun p -> Target.Project(p, None))
            |> List.ofSeq

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
let private optionsFor (checker: FSharpChecker) (target: Target) =
    match target with
    | Target.Script script ->
        let path = Path.GetFullPath script
        let sourceText = SourceText.ofString (File.ReadAllText path)

        let options, diagnostics =
            checker.GetProjectOptionsFromScript(path, sourceText, assumeDotNetFramework = false)
            |> Async.RunSynchronously

        // a reference the script host could not resolve leaves the script
        // half-typed, and most rules then stay silent; say so rather than
        // reporting a suspiciously clean file
        for d in diagnostics |> List.truncate 5 do
            eprintfn $"  (script reference: {d.Message})"

        Ok options
    | Target.Project(project, _) ->
        let argsSw = Stopwatch.StartNew()
        let fscResult = fscArgs project
        argsSw.Stop()
        printfn $"msbuild (build + argument query): {argsSw.ElapsedMilliseconds} ms"

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

/// Analyze and fix one compilation; returns its exit code.
let private runTarget (opts: Options) (showHeader: bool) (target: Target) =
    let onlyFile =
        match target with
        | Target.Project(_, only) -> only
        | Target.Script _ -> None

    if showHeader then
        let label =
            match target with
            | Target.Project(p, Some file) -> $"{Path.GetFileName file} (in {Path.GetFileName p})"
            | Target.Project(p, None) -> Path.GetFileName p
            | Target.Script s -> Path.GetFileName s

        printfn $"== {label} =="

    // analyzers may read the typed tree, which needs assembly contents
    let checker = FSharpChecker.Create(keepAssemblyContents = true)

    match optionsFor checker target with
    | Error message ->
        eprintfn $"{message}"
        1
    | Ok options ->
        let analyzers = cliAnalyzers ()
        printfn $"{analyzers.Length} analyzers, {options.SourceFiles.Length} files"

        // cross-file (API-changing) rule variants gate on this: they
        // stay silent in editors and in default runs
        if opts.ApiChanges then
            Environment.SetEnvironmentVariable("FSREF_API_CHANGES", "1")

        // Not worth skipping on a dry run: measured, the cost simply
        // moves to runPass's own ParseAndCheckProject, which is only
        // cheap here BECAUSE this call warmed FCS. One full project
        // typecheck is paid either way; this ordering at least reports
        // a broken project up front.
        let baselineSw = Stopwatch.StartNew()
        let baselineErrors = errorCount checker options
        baselineSw.Stop()
        printfn $"baseline project check: {baselineSw.ElapsedMilliseconds} ms"

        if baselineErrors > 0 then
            eprintfn $"The project has {baselineErrors} error(s) before any fix; fix those first:"

            for d in projectErrors checker options |> Array.truncate 5 do
                eprintfn $"  {d.FileName}({d.StartLine},{d.StartColumn}): {d.Message}"

            1
        else
            // asking for one file and getting edits in its callers would be
            // a surprise, and that is exactly what the cross-file rules do
            if opts.ApiChanges && onlyFile.IsSome then
                printfn "  (api pass skipped: a single file was named, and these fixes edit call sites elsewhere)"

            if opts.ApiChanges && onlyFile.IsNone then
                // iterated: a suggestion held back because its edits
                // nest inside another suggestion's applies next round
                let mutable apiPass = 0
                let mutable apiApplied = -1

                while apiPass < opts.MaxPasses && apiApplied <> 0 do
                    apiPass <- apiPass + 1
                    printfn $"api pass {apiPass}:"
                    apiApplied <- runApiPass checker options opts.Codes opts.DryRun
                    printfn $"  {apiApplied} api-changing fix(es) applied"

                    if opts.DryRun then
                        // nothing was written, so a second round would
                        // only repeat the same report
                        apiApplied <- 0
                    elif apiApplied > 0 then
                        checker.InvalidateConfiguration options

            let mutable pass = 0
            let mutable lastApplied = -1

            while pass < opts.MaxPasses && lastApplied <> 0 do
                pass <- pass + 1
                printfn $"pass {pass}:"

                lastApplied <-
                    runPass checker options analyzers opts.Codes opts.DryRun opts.ApiChanges opts.Jobs onlyFile

                let prefix = if opts.DryRun then "would be " else ""
                printfn $"  {lastApplied} fix(es) {prefix}applied"

                if opts.DryRun then
                    lastApplied <- 0 // a dry run never converges; stop after one pass

            if opts.DryRun then
                0
            else
                checker.InvalidateConfiguration options
                let finalErrors = errorCount checker options

                if finalErrors > baselineErrors then
                    eprintfn $"Applying introduced {finalErrors - baselineErrors} error(s) — please review the diff."

                    1
                else
                    printfn "done; project still checks clean"
                    0

[<EntryPoint>]
let main argv =
    match parseArgs argv with
    | Error message ->
        eprintfn $"{message}"
        2
    | Ok opts when opts.Target = "" ->
        eprintfn "Pass what to fix: a .fsproj, a .fsx, a solution, a directory, or a glob."
        eprintfn "  fsharp-refactor Your.fsproj --dry-run"
        2
    | Ok opts ->
        match resolveTargets opts.Target with
        | Error message ->
            eprintfn $"{message}"
            2
        | Ok targets ->
            let several = targets.Length > 1

            if several then
                printfn $"{targets.Length} compilations to work through"

            // each target stands alone: one project that will not build
            // must not hide the rest, so all of them run and the worst
            // exit code wins
            targets |> List.map (runTarget opts several) |> List.fold max 0
