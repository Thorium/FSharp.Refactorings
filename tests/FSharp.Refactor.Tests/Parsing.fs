/// Test helpers: parse an F# source string with FCS (no project needed) and
/// apply a suggested edit back to the source so tests can verify the result
/// still parses.
module FSharp.Refactor.Tests.Parsing

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let private checker = FSharpChecker.Create()

/// Parse under a chosen file name. A `.fsx` name makes FCS parse the text
/// as a script, which needs no leading namespace or module — parsing script
/// content as `.fs` fails with "Files in libraries or multiple-file
/// applications must begin with a namespace or module declaration".
let parseNamed (fileName: string) (source: string) : ParsedInput * ISourceText =
    let sourceText = SourceText.ofString source

    let parsingOptions =
        { FSharpParsingOptions.Default with
            SourceFiles = [| fileName |] }

    let result =
        checker.ParseFile(fileName, sourceText, parsingOptions)
        |> Async.RunSynchronously

    if result.ParseHadErrors then
        failwithf "Test input does not parse: %A" result.Diagnostics

    result.ParseTree, sourceText

/// Parse without failing on errors: returns the recovered tree and whether
/// the parser complained. Malformed input still yields a partial tree —
/// which is exactly what analyzers see in an editor mid-keystroke, so the
/// rules must survive it rather than throw.
let tryParseNamed (fileName: string) (source: string) : ParsedInput * bool * ISourceText =
    let sourceText = SourceText.ofString source

    let parsingOptions =
        { FSharpParsingOptions.Default with
            SourceFiles = [| fileName |] }

    let result =
        checker.ParseFile(fileName, sourceText, parsingOptions)
        |> Async.RunSynchronously

    result.ParseTree, result.ParseHadErrors, sourceText

/// True when the source string parses without errors under a chosen file name.
let parsesCleanlyNamed (fileName: string) (source: string) : bool =
    let sourceText = SourceText.ofString source

    let parsingOptions =
        { FSharpParsingOptions.Default with
            SourceFiles = [| fileName |] }

    let result =
        checker.ParseFile(fileName, sourceText, parsingOptions)
        |> Async.RunSynchronously

    not result.ParseHadErrors

/// Parse a standalone source string; fails the test on parse errors so a
/// broken test input is caught immediately.
let parse (source: string) : ParsedInput * ISourceText = parseNamed "Test.fs" source

/// True when the source string parses without errors.
let parsesCleanly (source: string) : bool = parsesCleanlyNamed "Test.fs" source

/// Parse and fully typecheck a source string as a script. Returns the parse
/// tree, source text, and check results (which may contain error diagnostics —
/// callers assert on them as needed).
let parseAndCheck (source: string) : ParsedInput * ISourceText * FSharpCheckFileResults =
    let sourceText = SourceText.ofString source

    let options, _ =
        // assumeDotNetFramework=false: resolve modern .NET references, so
        // APIs like Dictionary.TryAdd (absent from .NET Framework) typecheck
        checker.GetProjectOptionsFromScript("Test.fsx", sourceText, assumeDotNetFramework = false)
        |> Async.RunSynchronously

    let parseResults, answer =
        checker.ParseAndCheckFileInProject("Test.fsx", source.GetHashCode(), sourceText, options)
        |> Async.RunSynchronously

    match answer with
    | FSharpCheckFileAnswer.Succeeded checkResults -> parseResults.ParseTree, sourceText, checkResults
    | FSharpCheckFileAnswer.Aborted -> failwith $"Typechecking was aborted, calling parseAndCheck with source: {source}"

/// True when the source typechecks as a script without errors.
let typechecksCleanly (source: string) : bool =
    let _, _, checkResults = parseAndCheck source

    checkResults.Diagnostics
    |> Array.forall (fun d -> d.Severity <> FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error)

/// Replace `range` in `source` with `newText` (ranges are 1-based lines,
/// 0-based columns).
let applyEdit (source: string) (range: range) (newText: string) : string =
    let lines = source.Replace("\r\n", "\n").Split '\n'

    let before = [ for i in 0 .. range.StartLine - 2 -> lines.[i] ]

    let after = [ for i in range.EndLine .. lines.Length - 1 -> lines.[i] ]

    let startLinePrefix = lines.[range.StartLine - 1].Substring(0, range.StartColumn)
    let endLineSuffix = lines.[range.EndLine - 1].Substring range.EndColumn

    let patchedMiddle = startLinePrefix + newText + endLineSuffix

    String.concat "\n" (before @ [ patchedMiddle ] @ after)
