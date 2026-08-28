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
open FSharp.Compiler.Text
open FSharp.Refactor

type private Options =
    {
        Target: string
        ShowHelp: bool
        Codes: Set<string> option
        /// Narrows `Codes` once parsing is done, so the two flags combine the
        /// same way whichever order they were given in.
        Categories: Set<RuleCatalog.Category> option
        DryRun: bool
        ApiChanges: bool
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
  --api-changes         also apply cross-file fixes that change internal or
                        public signatures, rewriting call sites project-wide.
                        Held back and merely counted without this
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
        parseArgsLoop
            { opts with
                Codes = Some(codes.Split(',') |> Array.map _.Trim() |> Set.ofArray) }
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
          Categories = None
          DryRun = false
          ApiChanges = false
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
    let t = tfm.ToLowerInvariant()
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
let private fscArgs (chosenFramework: string) (projectPath: string) =
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
    let assembly = typeof<FSharp.Refactor.RedundantParens.Suggestion>.Assembly

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
/// A rule's kind, padded so the file paths after it line up. "correctness"
/// and "performance" are the longest at eleven.
let private kindColumn (code: string) =
    let kind = RuleCatalog.name (RuleCatalog.categoryOf code)
    $"[{kind}]".PadRight 13

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
            // An edit that replaces text with itself changes nothing, yet
            // counting it as applied asks for another pass — and the next
            // pass offers it again, until --max-passes runs out. A rule
            // producing one is buggy, but the loop is ours to not spin.
            let changesSomething = f.ToText.Replace("\r", "") <> f.FromText.Replace("\r", "")

            if changesSomething && not (overlaps f.FromRange) then
                let lines = current.Split '\n'

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

                    printfn
                        $"  {code} {kindColumn code} {Path.GetFileName file}({f.FromRange.StartLine},{f.FromRange.StartColumn})"

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
                printfn
                    $"  {s.Code} {kindColumn s.Code} {s.FunctionName}: held back this round (edits nest inside another change)"
            else
                printfn $"  {s.Code} {kindColumn s.Code} {s.FunctionName}: {s.Edits.Length} edit(s) across the project"

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

    // naming one source file means analyzing its project — the references
    // and the files before it are what give its names meaning — but
    // sweeping only that file
    let filesToSweep =
        match onlyFile with
        | Some only ->
            options.SourceFiles
            |> Array.filter (fun f -> String.Equals(Path.GetFullPath f, only, StringComparison.OrdinalIgnoreCase))
        | None -> options.SourceFiles

    printf $"sweeping {filesToSweep.Length} file(s)... "
    Console.Out.Flush()
    let sweepSw = Stopwatch.StartNew()

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

        match solutions with
        | solution :: _ -> projectsInSolution solution |> List.map (fun p -> Target.Project(p, None))
        | [] ->
            FileWalk.files "*.fsproj" dir
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
let private optionsFor (checker: FSharpChecker) (chosenFramework: string) (target: Target) =
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
        // announced BEFORE it starts: this step can take a minute, and a
        // line that only appears afterwards is no help while you are
        // staring at a silent terminal wondering whether it is stuck
        printf "building and reading compiler arguments... "
        Console.Out.Flush()
        let argsSw = Stopwatch.StartNew()
        let fscResult = fscArgs chosenFramework project
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
                File.WriteAllText(path, original)
                1
            else
                0
        with
        | :? IOException
        | :? UnauthorizedAccessException -> 0)

/// Analyze and fix one compilation; returns its exit code.
let private runTarget (opts: Options) (showHeader: bool) (target: Target) =
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

    // analyzers may read the typed tree, which needs assembly contents
    let checker = FSharpChecker.Create(keepAssemblyContents = true)

    match optionsFor checker opts.Framework target with
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
        // before anything is written, so a framework pass that turns out to
        // break another one can be undone rather than merely reported
        let snapshot =
            if opts.DryRun || not (isMultiTargeted target) then
                Map.empty
            else
                takeSnapshot options.SourceFiles

        printf "typechecking the project... "
        Console.Out.Flush()
        let baselineSw = Stopwatch.StartNew()
        let baselineErrors = errorCount checker options
        baselineSw.Stop()
        printfn $"{baselineSw.ElapsedMilliseconds} ms"

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
                // The check above only covers the framework we analysed. A
                // multi-targeted project has others, and a fix valid for one
                // can fail on another, so build the lot before claiming
                // success.
                elif isMultiTargeted target then
                    match target with
                    | Target.Project(project, _) ->
                        printfn "verifying every target framework..."

                        match buildAllFrameworks project with
                        | Ok() ->
                            printfn "done; every target framework still builds"
                            0
                        | Error output ->
                            // This pass changed code the other frameworks
                            // also compile — shared code, outside any #if —
                            // and offered something only this framework can
                            // resolve. Reporting is not enough: put the
                            // files back, or the caller is left with a
                            // project that does not build.
                            let restored = restoreSnapshot snapshot

                            eprintfn
                                $"Applying broke a target framework this run did not analyze, so the {restored} file(s) it changed were put back:"

                            eprintfn $"{output}"
                            1
                    | Target.Script _ ->
                        printfn "done; project still checks clean"
                        0
                else
                    printfn "done; project still checks clean"
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
    // no arguments at all is a question, not a mistake: show the help
    | Ok opts when opts.Target = "" ->
        printfn $"{helpText}"
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

            // A multi-targeted project is really several compilations: each
            // framework activates its own #if branches, and code behind
            // another framework's is not in the parse tree at all. So work
            // through them rather than making the caller name each one —
            // narrowest first, so the fixes valid everywhere land before any
            // that only suit a wider surface. The final all-framework build
            // is what catches a fix that does not generalise.
            let runOne target =
                match opts.Framework, frameworksOf target with
                | "", (_ :: _ :: _ as frameworks) ->
                    printfn $"{Path.GetFileName opts.Target}: {frameworks.Length} target frameworks"

                    frameworks
                    |> List.map (fun tfm -> runTarget { opts with Framework = tfm } true target)
                    |> List.fold max 0
                | _ -> runTarget opts several target

            targets |> List.map runOne |> List.fold max 0
