/// Applies FSharp.Refactorings quick fixes from the command line.
///
///     dotnet tool install --global fsharp-refactorings-apply
///     fsharp-refactor --project Your.fsproj
///
/// (from this repository:
/// `dotnet run --project src/FSharp.Refactorings.Apply -- --project Your.fsproj`)
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
/// Options:
///     --project <path.fsproj>   required
///     --codes FR0002,FR0031    only apply these rule codes
///     --dry-run                report what would be applied, change nothing
///     --api-changes            also apply CROSS-FILE fixes (rules that
///                              rewrite call sites of internal/public
///                              symbols across the project); without it
///                              those fixes are held back and counted
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
    { Project: string
      Codes: Set<string> option
      DryRun: bool
      ApiChanges: bool
      MaxPasses: int }

[<TailCall>]
let rec private parseArgsLoop opts args =
    match args with
    | [] -> Ok opts
    | "--project" :: path :: rest -> parseArgsLoop { opts with Project = path } rest
    | "--codes" :: codes :: rest ->
        parseArgsLoop
            { opts with
                Codes = Some(codes.Split(',') |> Array.map _.Trim() |> Set.ofArray) }
            rest
    | "--dry-run" :: rest -> parseArgsLoop { opts with DryRun = true } rest
    | "--api-changes" :: rest -> parseArgsLoop { opts with ApiChanges = true } rest
    | "--max-passes" :: n :: rest ->
        match Int32.TryParse n with
        | true, passes when passes > 0 -> parseArgsLoop { opts with MaxPasses = passes } rest
        | _ -> Error $"--max-passes needs a positive number, got '{n}'"
    | unknown :: _ -> Error $"Unknown argument '{unknown}'"

let private parseArgs (argv: string[]) =
    parseArgsLoop
        { Project = ""
          Codes = None
          DryRun = false
          ApiChanges = false
          MaxPasses = 5 }
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
    let isSdkStyle =
        try
            (File.ReadAllText projectPath).Contains "Sdk="
        with _ ->
            true

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
    let buildExit, buildOut, buildErr = run $"build \"{projectPath}\""

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
                $"msbuild \"{projectPath}\" -t:Rebuild -p:BuildProjectReferences=false -p:ProvideCommandLineArgs=true -p:SkipCompilerExecution=true --getItem:FscCommandLineArgs"

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
    =
    let projectResults = checker.ParseAndCheckProject options |> Async.RunSynchronously

    // every accepted fix, grouped by the file it EDITS: a fix range names
    // its file, so a cross-file (API-changing) rule can edit call sites
    // anywhere in the project — but only under --api-changes. Keys are
    // normalized full paths: two spellings of one file must land in ONE
    // group, or the second write would clobber the first group's edits.
    let editsByFile =
        System.Collections.Generic.Dictionary<string, ResizeArray<string * Fix>>(StringComparer.OrdinalIgnoreCase)

    let mutable crossFileSkipped = 0

    for file in options.SourceFiles do
        let text = File.ReadAllText file
        let sourceText = SourceText.ofString text

        let parseResults, checkAnswer =
            checker.ParseAndCheckFileInProject(file, 0, sourceText, options)
            |> Async.RunSynchronously

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

            let messages =
                analyzers
                |> List.collect (fun m ->
                    try
                        m.Invoke(null, [| box context |]) :?> Async<Message list>
                        |> Async.RunSynchronously
                    with
                    // an analyzer that throws must not take the run down, but
                    // say so: a silently skipped rule looks like a clean file
                    | :? TargetInvocationException as ex ->
                        eprintfn $"  (analyzer {m.Name} failed: {ex.InnerException.Message})"
                        []
                    | :? InvalidCastException as ex ->
                        eprintfn $"  (analyzer {m.Name} has an unexpected signature: {ex.Message})"
                        [])
                |> List.filter (fun msg ->
                    not msg.Fixes.IsEmpty
                    && codes |> Option.forall (fun wanted -> wanted.Contains msg.Code))

            for msg in messages do
                for f in msg.Fixes do
                    let target =
                        Path.GetFullPath(
                            if String.IsNullOrEmpty f.FromRange.FileName then
                                file
                            else
                                f.FromRange.FileName
                        )

                    let sameFile =
                        String.Equals(target, Path.GetFullPath file, StringComparison.OrdinalIgnoreCase)

                    if sameFile || apiChanges then
                        match editsByFile.TryGetValue target with
                        | true, existing -> existing.Add(msg.Code, f)
                        | false, _ ->
                            let fresh = ResizeArray()
                            fresh.Add(msg.Code, f)
                            editsByFile.[target] <- fresh
                    else
                        crossFileSkipped <- crossFileSkipped + 1
        | FSharpCheckFileAnswer.Aborted -> ()

    if crossFileSkipped > 0 then
        printfn $"  ({crossFileSkipped} cross-file fix(es) held back — rerun with --api-changes to apply them)"

    applyEditGroups dryRun editsByFile

[<EntryPoint>]
let main argv =
    match parseArgs argv with
    | Error message ->
        eprintfn $"{message}"
        2
    | Ok opts when opts.Project = "" || not (File.Exists opts.Project) ->
        eprintfn "Pass --project <path to an existing .fsproj>."
        2
    | Ok opts ->
        match fscArgs opts.Project with
        | Error message ->
            eprintfn $"{message}"
            1
        | Ok args ->
            // analyzers may read the typed tree, which needs assembly contents
            let checker = FSharpChecker.Create(keepAssemblyContents = true)
            let projectDir = Path.GetDirectoryName(Path.GetFullPath opts.Project)

            // FCS leaves SourceFiles empty for command-line args; partition
            // and rebase the relative paths MSBuild emits ourselves
            let sourceExtensions = [| ".fs"; ".fsi"; ".fsx" |]

            let isSource (arg: string) =
                not (arg.StartsWith '-')
                && sourceExtensions
                   |> Array.exists (fun ext -> arg.EndsWith(ext, StringComparison.OrdinalIgnoreCase))

            let sources, otherArgs = args |> Array.partition isSource

            let absoluteSources =
                sources
                |> Array.map (fun s ->
                    if Path.IsPathRooted s then
                        s
                    else
                        Path.Combine(projectDir, s))

            let options =
                { checker.GetProjectOptionsFromCommandLineArgs(Path.GetFullPath opts.Project, otherArgs) with
                    SourceFiles = absoluteSources }

            let analyzers = cliAnalyzers ()
            printfn $"{analyzers.Length} analyzers, {options.SourceFiles.Length} files"

            // cross-file (API-changing) rule variants gate on this: they
            // stay silent in editors and in default runs
            if opts.ApiChanges then
                Environment.SetEnvironmentVariable("FSREF_API_CHANGES", "1")

            let baselineErrors = errorCount checker options

            if baselineErrors > 0 then
                eprintfn $"The project has {baselineErrors} error(s) before any fix; fix those first:"

                for d in projectErrors checker options |> Array.truncate 5 do
                    eprintfn $"  {d.FileName}({d.StartLine},{d.StartColumn}): {d.Message}"

                1
            else
                if opts.ApiChanges then
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
                    lastApplied <- runPass checker options analyzers opts.Codes opts.DryRun opts.ApiChanges
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
                        eprintfn
                            $"Applying introduced {finalErrors - baselineErrors} error(s) — please review the diff."

                        1
                    else
                        printfn "done; project still checks clean"
                        0
