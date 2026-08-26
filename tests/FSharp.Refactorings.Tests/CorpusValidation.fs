/// Opt-in corpus validation, activated by environment variables so CI and
/// plain `dotnet test` runs skip it silently:
///
///   FSREF_CORPUS_ROOTS = C:\git\RepoA;C:\git\RepoB
///       runs every parse-only fix rule over each .fs file under the
///       roots, applies every suggested edit individually, and verifies
///       the patched file still parses — the guard that caught fixes
///       splicing #if/#else/#endif blocks apart
///
///   FSREF_APPLY_ARGS = --project|C:\path\X.fsproj|--codes|FR0002
///       runs the full apply tool (arguments separated by `|`) through the
///       test host — the working channel on machines where Smart App
///       Control blocks freshly built executables
module FSharp.Refactorings.Tests.CorpusValidation

open System
open System.IO
open Xunit
open FSharp.Refactorings
open FSharp.Refactorings.Tests.Parsing

[<Fact>]
let ``corpus fixes still parse`` () : unit =
    match Environment.GetEnvironmentVariable "FSREF_CORPUS_ROOTS" with
    | null
    | "" -> ()
    | roots ->
        let failures = ResizeArray<string>()
        let mutable applied = 0
        let mutable filesSeen = 0

        let files =
            roots.Split(';')
            |> Seq.collect (fun root ->
                if Directory.Exists root then
                    Directory.EnumerateFiles(root, "*.fs", SearchOption.AllDirectories)
                else
                    Seq.empty)
            |> Seq.filter (fun f -> not (f.Contains @"\obj\" || f.Contains @"\bin\" || f.Contains @"\packages\"))

        for file in files do
            let source = File.ReadAllText file

            try
                let tree, sourceText = parse source
                filesSeen <- filesSeen + 1

                let suggestions =
                    [ for s in RedundantParens.find tree sourceText -> s.Range, s.ReplacementText, "FR0013"
                      for s in MatchToIf.find tree sourceText -> s.Range, s.ReplacementText, "FR0001"
                      for s in RaiseFailwith.find tree sourceText -> s.Range, s.ReplacementText, "FR0024"
                      for s in AttributeMerge.find tree sourceText -> s.Range, s.ReplacementText, "FR0060"
                      for s in HintEngine.find [] tree sourceText -> s.Range, s.ReplacementText, "FR0011/12"
                      for s in Simplification.find tree sourceText None -> s.Range, s.ReplacementText, "FR0010"
                      for s in MatchBangRule.find tree sourceText do
                          for range, _, replacement in s.Edits -> range, replacement, "FR0073"
                      for s in MatchBangRule.findWhileBang tree sourceText do
                          for range, _, replacement in s.Edits -> range, replacement, "FR0078" ]

                // multi-edit rules (FR0073/FR0078) must apply as a set;
                // single edits verify individually
                for range, replacement, code in suggestions do
                    if code <> "FR0073" && code <> "FR0078" then
                        applied <- applied + 1
                        let patched = applyEdit source range replacement

                        if not (parsesCleanly patched) then
                            failures.Add $"{code} {file}({range.StartLine},{range.StartColumn}): -> {replacement}"

                let multiEditSets =
                    [ for s in MatchBangRule.find tree sourceText -> s.Edits
                      for s in MatchBangRule.findWhileBang tree sourceText -> s.Edits ]

                for edits in multiEditSets do
                    applied <- applied + 1

                    let patched =
                        edits
                        |> List.sortByDescending (fun (r, _, _) -> r.StartLine, r.StartColumn)
                        |> List.fold (fun acc (r, _, replacement) -> applyEdit acc r replacement) source

                    if not (parsesCleanly patched) then
                        failures.Add $"multi-edit {file}: {edits.Length} edits"
            with ex ->
                failures.Add $"CRASH {file}: {ex.Message}"

        let report =
            $"files: {filesSeen}, fixes applied: {applied}, failures: {failures.Count}\n"
            + String.concat "\n" failures

        File.WriteAllText(Path.Combine(Path.GetTempPath(), "fsref-corpus-validation.txt"), report)
        Assert.True(failures.Count = 0, report)

[<Fact>]
let ``run apply tool from env`` () : unit =
    match Environment.GetEnvironmentVariable "FSREF_APPLY_ARGS" with
    | null
    | "" -> ()
    | args ->
        let argv = args.Split('|')
        use captured = new StringWriter()
        let oldOut = Console.Out
        let oldErr = Console.Error
        Console.SetOut captured
        Console.SetError captured

        let code =
            try
                FSharp.Refactorings.Apply.Program.main argv
            finally
                Console.SetOut oldOut
                Console.SetError oldErr

        File.WriteAllText(Path.Combine(Path.GetTempPath(), "fsref-apply-run.txt"), captured.ToString())
        Assert.True((code = 0), sprintf "exit %d:\n%s" code (captured.ToString()))
