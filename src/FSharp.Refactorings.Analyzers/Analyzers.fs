/// FSharp.Analyzers.SDK entry points. Each refactoring is exposed twice:
/// once for editors (FsAutoComplete/Ionide) and once for the CLI
/// (fsharp-analyzers tool, usable in CI). The logic itself lives in the
/// per-refactoring modules; this file only builds the diagnostic messages
/// and applies the optional per-repository configuration
/// (fsharprefactorings.json), which can disable rules by code or name.
module FSharp.Refactorings.Analyzers

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK

[<Literal>]
let private HelpBase = "https://github.com/Thorium/FSharp.Refactorings"

let private fix (range: range) (original: string) (replacement: string) : Fix =
    { FromRange = range
      FromText = original
      ToText = replacement }

let private hint (code: string) (message: string) (range: range) (fixes: Fix list) : Message =
    { Type = "FSharp.Refactorings"
      Message = message
      Code = code
      Severity = Severity.Hint
      Range = range
      Fixes = fixes }

/// Run a typed rule only when check results are available.
let private whenChecked (ctx: EditorContext) (produce: FSharpCheckFileResults -> Message list) : Message list =
    ctx.CheckFileResults |> Option.map produce |> Option.defaultValue []

/// Run a rule's message builder only when the configuration enables the rule
/// for the analyzed file. A disabled rule skips all analysis work.
let private whenEnabled (fileName: string) (code: string) (name: string) (produce: unit -> Message list) =
    async {
        return
            if Configuration.isRuleEnabled fileName code name then
                produce ()
            else
                []
    }

// ---- FR0001 MatchToIf ----

let private matchToIfMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    MatchToIf.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0001"
            "This boolean match expression can be written as an if-else expression."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("MatchToIf", "Rewrite a boolean match expression as if-else", HelpBase)>]
let matchToIfEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0001" "MatchToIf" (fun () ->
        matchToIfMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("MatchToIf", "Rewrite a boolean match expression as if-else", HelpBase)>]
let matchToIfCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0001" "MatchToIf" (fun () ->
        matchToIfMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0002 OptionModule ----

let private optionModuleMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    OptionModule.find parseTree source checkResults
    |> List.map (fun s ->
        let message =
            if s.Target = "" then
                "This match expression is the identity on an option and can be removed."
            else
                $"This match expression can be written with %s{s.Target}."

        hint "FR0002" message s.Range [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("OptionModule", "Rewrite Some/None matching with Option-module functions", HelpBase)>]
let optionModuleEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0002" "OptionModule" (fun () ->
        whenChecked ctx (optionModuleMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("OptionModule", "Rewrite Some/None matching with Option-module functions", HelpBase)>]
let optionModuleCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0002" "OptionModule" (fun () ->
        optionModuleMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0003 Composition ----

let private compositionMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    Composition.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0003"
            "This lambda is a function composition and can be written with >>."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("Composition", "Extract a function composition from a lambda", HelpBase)>]
let compositionEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0003" "Composition" (fun () ->
        compositionMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("Composition", "Extract a function composition from a lambda", HelpBase)>]
let compositionCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0003" "Composition" (fun () ->
        compositionMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0004 ConversionMove ----

let private conversionMoveMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    ConversionMove.find parseTree source
    |> List.map (fun s ->
        let message =
            if s.Eliminated then
                "This collection conversion is unnecessary before a consuming operation."
            else
                "This collection conversion can be moved after the operation, avoiding an intermediate collection."

        hint "FR0004" message s.Range [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("ConversionMove", "Move or drop List/Seq/Array conversions in pipelines", HelpBase)>]
let conversionMoveEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0004" "ConversionMove" (fun () ->
        conversionMoveMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("ConversionMove", "Move or drop List/Seq/Array conversions in pipelines", HelpBase)>]
let conversionMoveCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0004" "ConversionMove" (fun () ->
        conversionMoveMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0005 CeStrip ----

let private ceStripMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    CeStrip.find parseTree source
    |> List.map (fun s ->
        let message =
            match s.Kind with
            | CeStrip.StripKind.WithRunner -> "This async wrapping is immediately run and can be removed."
            | CeStrip.StripKind.Forwarded -> "This async wrapping does nothing and can be removed."
            | CeStrip.StripKind.TaskFromResult ->
                "This task wrapping only wraps a value and can be written with Task.FromResult."

        hint "FR0005" message s.Range [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("CeStrip", "Strip computation-expression wrapping that does nothing", HelpBase)>]
let ceStripEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0005" "CeStrip" (fun () ->
        ceStripMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("CeStrip", "Strip computation-expression wrapping that does nothing", HelpBase)>]
let ceStripCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0005" "CeStrip" (fun () ->
        ceStripMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0006 ActivePattern ----

let private activePatternMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    ActivePattern.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0006"
            ($"This guard can be extracted into an active pattern (|%s{s.PatternName}|_|).")
            s.ClauseRange
            [ fix s.ClauseRange s.OriginalClauseText s.ClauseText
              fix s.InsertRange "" s.InsertText ])

[<EditorAnalyzer("ActivePattern", "Extract a when-guard into an active pattern", HelpBase)>]
let activePatternEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0006" "ActivePattern" (fun () ->
        activePatternMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("ActivePattern", "Extract a when-guard into an active pattern", HelpBase)>]
let activePatternCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0006" "ActivePattern" (fun () ->
        activePatternMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0007 MutableRemoval ----

let private mutableRemovalMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    MutableRemoval.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0007"
            ($"'%s{s.Name}' is never mutated; the mutable keyword can be removed.")
            s.Range
            [ fix s.Range s.OriginalText "" ])

[<EditorAnalyzer("MutableRemoval", "Remove mutable from never-mutated local bindings", HelpBase)>]
let mutableRemovalEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0007" "MutableRemoval" (fun () ->
        whenChecked ctx (mutableRemovalMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("MutableRemoval", "Remove mutable from never-mutated local bindings", HelpBase)>]
let mutableRemovalCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0007" "MutableRemoval" (fun () ->
        mutableRemovalMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0008 TupleParams ----

let private tupleParamsMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    TupleParams.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0008"
            ($"Private function '%s{s.FunctionName}' takes a tuple; curried parameters are more idiomatic F#.")
            s.DefRange
            (s.Edits |> List.map (fun e -> fix e.Range e.Original e.Replacement)))

[<EditorAnalyzer("TupleParams", "Convert private tupled functions to curried parameters", HelpBase)>]
let tupleParamsEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0008" "TupleParams" (fun () ->
        whenChecked ctx (tupleParamsMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("TupleParams", "Convert private tupled functions to curried parameters", HelpBase)>]
let tupleParamsCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0008" "TupleParams" (fun () ->
        tupleParamsMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0009 ResultModule ----

let private resultModuleMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    ResultModule.find parseTree source checkResults
    |> List.map (fun s ->
        let message =
            if s.Target = "" then
                "This match expression is the identity on a Result and can be removed."
            else
                $"This match expression can be written with %s{s.Target}."

        hint "FR0009" message s.Range [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("ResultModule", "Rewrite Ok/Error matching with Result-module functions", HelpBase)>]
let resultModuleEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0009" "ResultModule" (fun () ->
        whenChecked ctx (resultModuleMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("ResultModule", "Rewrite Ok/Error matching with Result-module functions", HelpBase)>]
let resultModuleCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0009" "ResultModule" (fun () ->
        resultModuleMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0010 Simplification ----

let private simplificationMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    Simplification.find parseTree source checkResults
    |> List.map (fun s ->
        let message =
            match s.Kind with
            | Simplification.SimplificationKind.BooleanIdentity ->
                "This if-expression just returns the condition and can be simplified."
            | Simplification.SimplificationKind.OptionComparison ->
                "Comparing against None can be written with the isNone/isSome function."
            | Simplification.SimplificationKind.Emptiness ->
                "Comparing length against zero can be written with isEmpty (and avoids forcing a full sequence)."

        hint "FR0010" message s.Range [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("Simplification", "Simplify boolean, None-comparison, and emptiness idioms", HelpBase)>]
let simplificationEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0010" "Simplification" (fun () ->
        simplificationMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

[<CliAnalyzer("Simplification", "Simplify boolean, None-comparison, and emptiness idioms", HelpBase)>]
let simplificationCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0010" "Simplification" (fun () ->
        simplificationMessages ctx.ParseFileResults.ParseTree ctx.SourceText (Some ctx.CheckFileResults))

// ---- FR0011 StructActivePattern ----

let private structActivePatternMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    StructActivePattern.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0011"
            (sprintf
                "Active pattern (%s) can return a struct option ([<return: Struct>]), avoiding an allocation per match attempt."
                s.PatternName)
            s.NameRange
            (s.Edits |> List.map (fun e -> fix e.Range e.Original e.Replacement)))

[<EditorAnalyzer("StructActivePattern", "Make trivial partial active patterns struct-returning", HelpBase)>]
let structActivePatternEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0011" "StructActivePattern" (fun () ->
        whenChecked ctx (structActivePatternMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("StructActivePattern", "Make trivial partial active patterns struct-returning", HelpBase)>]
let structActivePatternCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0011" "StructActivePattern" (fun () ->
        structActivePatternMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0012 Hints ----

let private hintMessages (extraRules: string list) (parseTree: ParsedInput) (source: ISourceText) : Message list =
    HintEngine.find extraRules parseTree source
    |> List.map (fun s ->
        hint
            "FR0012"
            ($"This expression can be simplified (%s{s.Rule}).")
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("Hints", "Term-rewriting hints (fsharplint-style rules)", HelpBase)>]
let hintsEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0012" "Hints" (fun () ->
        hintMessages (Configuration.hintsFor ctx.FileName) ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("Hints", "Term-rewriting hints (fsharplint-style rules)", HelpBase)>]
let hintsCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0012" "Hints" (fun () ->
        hintMessages (Configuration.hintsFor ctx.FileName) ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0013 RedundantParens ----

let private redundantParensMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    RedundantParens.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0013"
            "Redundant parentheses around a single atomic argument."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("RedundantParens", "Drop redundant parentheses around single atomic arguments", HelpBase)>]
let redundantParensEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0013" "RedundantParens" (fun () ->
        redundantParensMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("RedundantParens", "Drop redundant parentheses around single atomic arguments", HelpBase)>]
let redundantParensCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0013" "RedundantParens" (fun () ->
        redundantParensMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0014 DictTryGet ----

let private dictTryGetMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    DictTryGet.find parseTree source checkResults
    |> List.map (fun s ->
        let message =
            if s.Concurrent then
                "ContainsKey followed by the indexer is a race on ConcurrentDictionary; use a single TryGetValue."
            else
                "ContainsKey followed by the indexer looks the key up twice; use a single TryGetValue."

        hint "FR0014" message s.Range [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("DictTryGet", "Replace ContainsKey-plus-indexer with TryGetValue", HelpBase)>]
let dictTryGetEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0014" "DictTryGet" (fun () ->
        whenChecked ctx (dictTryGetMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("DictTryGet", "Replace ContainsKey-plus-indexer with TryGetValue", HelpBase)>]
let dictTryGetCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0014" "DictTryGet" (fun () ->
        dictTryGetMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0015 RegexUsage ----

let private regexUsageMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    RegexUsage.find parseTree source
    |> List.map (fun s ->
        let message =
            match s.Kind with
            | RegexUsage.RegexSuggestionKind.StringOperation ->
                "This literal regex pattern is a plain string operation."
            | RegexUsage.RegexSuggestionKind.HoistFromLoop ->
                "This Regex call re-parses its pattern on every loop iteration; construct one Regex before the loop and reuse it."

        hint "FR0015" message s.Range (s.Edits |> List.map (fun (r, o, t) -> fix r o t)))

[<EditorAnalyzer("RegexUsage", "Simplify literal regex patterns; hoist Regex construction out of loops", HelpBase)>]
let regexUsageEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0015" "RegexUsage" (fun () ->
        regexUsageMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("RegexUsage", "Simplify literal regex patterns; hoist Regex construction out of loops", HelpBase)>]
let regexUsageCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0015" "RegexUsage" (fun () ->
        regexUsageMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0016 StructDu ----

let private structDuMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    StructDu.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0016"
            (sprintf
                "Union '%s' holds only small value types; [<Struct>] avoids a heap allocation per value."
                s.TypeName)
            s.InsertRange
            [ fix s.InsertRange "" s.InsertText ])

[<EditorAnalyzer("StructDu", "Mark small discriminated unions with Struct", HelpBase)>]
let structDuEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0016" "StructDu" (fun () ->
        structDuMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("StructDu", "Mark small discriminated unions with Struct", HelpBase)>]
let structDuCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0016" "StructDu" (fun () ->
        structDuMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0022 DuFieldNames ----

let private duFieldNamesMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    DuFieldNames.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0022"
            (sprintf
                "Union case '%s' can name its fields (%s) after the names its match sites already use."
                s.CaseName
                (String.concat ", " s.Names))
            s.Range
            (s.Edits
             |> List.map (fun (r, original, replacement) -> fix r original replacement)))

[<EditorAnalyzer("DuFieldNames", "Name private union case fields after their match sites", HelpBase)>]
let duFieldNamesEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0022" "DuFieldNames" (fun () ->
        duFieldNamesMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("DuFieldNames", "Name private union case fields after their match sites", HelpBase)>]
let duFieldNamesCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0022" "DuFieldNames" (fun () ->
        duFieldNamesMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0023 ParamOrder ----

let private paramOrderMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    ParamOrder.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0023"
            (sprintf
                "'%s' takes its varying argument first; the fix swaps the definition to data-last order and rewrites every call site, so 'fun x -> %s x k' lambdas become the partial application '%s k'."
                s.FunctionName
                s.FunctionName
                s.FunctionName)
            s.DefRange
            (s.Edits
             |> List.map (fun (r, original, replacement) -> fix r original replacement)))

[<EditorAnalyzer("ParamOrder", "Reorder private function parameters data-last", HelpBase)>]
let paramOrderEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0023" "ParamOrder" (fun () ->
        whenChecked ctx (paramOrderMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("ParamOrder", "Reorder private function parameters data-last", HelpBase)>]
let paramOrderCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0023" "ParamOrder" (fun () ->
        paramOrderMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0017 AsyncIgnore ----

let private discardedAsyncMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    AsyncIgnore.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0017"
            (sprintf
                "'%s' is an Async computation: ignore discards it without running it. Use do! %s |> Async.Ignore to await it, or Async.Start to fire and forget."
                s.Name
                s.Name)
            s.Range
            [])

[<EditorAnalyzer("AsyncIgnore", "Flag Async computations discarded with ignore", HelpBase)>]
let asyncIgnoreEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0017" "AsyncIgnore" (fun () ->
        whenChecked ctx (discardedAsyncMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("AsyncIgnore", "Flag Async computations discarded with ignore", HelpBase)>]
let asyncIgnoreCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0017" "AsyncIgnore" (fun () ->
        discardedAsyncMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0018 DictTryAdd ----

let private dictTryAddMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    DictTryGet.findTryAdd parseTree source checkResults
    |> List.map (fun s ->
        let message =
            if s.Concurrent then
                "Check-then-add is a race on ConcurrentDictionary; use a single TryAdd."
            else
                "ContainsKey followed by an indexer add looks the key up twice; use a single TryAdd."

        hint "FR0018" message s.Range [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("DictTryAdd", "Replace check-then-add with TryAdd", HelpBase)>]
let dictTryAddEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0018" "DictTryAdd" (fun () ->
        whenChecked ctx (dictTryAddMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("DictTryAdd", "Replace check-then-add with TryAdd", HelpBase)>]
let dictTryAddCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0018" "DictTryAdd" (fun () ->
        dictTryAddMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0019 / FR0020 ObjectRules ----

let private objectRulesMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    let equalsSuggestions, ctorSuggestions = ObjectRules.find parseTree source

    let equalsMessages =
        equalsSuggestions
        |> List.map (fun s ->
            hint
                "FR0019"
                (sprintf
                    "Type '%s' overrides Equals without overriding GetHashCode; hash-based collections will misbehave."
                    s.TypeName)
                s.Range
                [])

    let ctorMessages =
        ctorSuggestions
        |> List.map (fun s ->
            hint
                "FR0020"
                (sprintf
                    "Abstract member '%s' is used during construction; the override runs before the derived class is initialized."
                    s.MemberName)
                s.Range
                [])

    equalsMessages @ ctorMessages

[<EditorAnalyzer("ObjectRules", "Equals/GetHashCode pairing and ctor-time abstract calls", HelpBase)>]
let objectRulesEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0019" "ObjectRules" (fun () ->
        objectRulesMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("ObjectRules", "Equals/GetHashCode pairing and ctor-time abstract calls", HelpBase)>]
let objectRulesCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0019" "ObjectRules" (fun () ->
        objectRulesMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0024 RaiseFailwith ----

let private raiseFailwithMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    RaiseFailwith.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0024"
            "raise with a plain Exception is exactly failwith; the raised type and message are unchanged."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("RaiseFailwith", "Rewrite raise (Exception msg) as failwith", HelpBase)>]
let raiseFailwithEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0024" "RaiseFailwith" (fun () ->
        raiseFailwithMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("RaiseFailwith", "Rewrite raise (Exception msg) as failwith", HelpBase)>]
let raiseFailwithCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0024" "RaiseFailwith" (fun () ->
        raiseFailwithMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0025 OptionOfObj ----

let private optionOfObjMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    OptionOfObj.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0025"
            ($"This null test wraps the value into an option and can be written with %s{s.ModuleName}.ofObj.")
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("OptionOfObj", "Rewrite null-test-and-wrap as Option.ofObj", HelpBase)>]
let optionOfObjEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0025" "OptionOfObj" (fun () ->
        whenChecked ctx (optionOfObjMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("OptionOfObj", "Rewrite null-test-and-wrap as Option.ofObj", HelpBase)>]
let optionOfObjCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0025" "OptionOfObj" (fun () ->
        optionOfObjMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0026 AutoProperty ----

let private autoPropertyMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    AutoProperty.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0026"
            (sprintf
                "Property '%s' is a mutable backing field with trivial accessors; 'member val' says the same in one line."
                s.PropertyName)
            s.Range
            (s.Edits
             |> List.map (fun (r, original, replacement) -> fix r original replacement)))

[<EditorAnalyzer("AutoProperty", "Collapse trivial get/set with a backing field to member val", HelpBase)>]
let autoPropertyEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0026" "AutoProperty" (fun () ->
        autoPropertyMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("AutoProperty", "Collapse trivial get/set with a backing field to member val", HelpBase)>]
let autoPropertyCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0026" "AutoProperty" (fun () ->
        autoPropertyMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0027 ClosureCapture ----

let private closureCaptureMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    ClosureCapture.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0027"
            (sprintf
                "This handler captures '%s', so the %s subscription keeps the whole object alive until the handler is removed. If the object is large, bind the needed values to locals before the lambda, or keep and dispose the subscription."
                s.CapturedName
                s.SinkName)
            s.Range
            [])

[<EditorAnalyzer("ClosureCapture", "Note this-capturing handlers given to event/observable sinks", HelpBase)>]
let closureCaptureEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0027" "ClosureCapture" (fun () ->
        whenChecked ctx (closureCaptureMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("ClosureCapture", "Note this-capturing handlers given to event/observable sinks", HelpBase)>]
let closureCaptureCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0027" "ClosureCapture" (fun () ->
        closureCaptureMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0028 QueryInLoop ----

let private queryInLoopMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    QueryInLoop.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0028"
            (sprintf
                "'%s' is an IQueryable iterated inside another loop: each outer iteration executes a separate database query (N+1). Materialize it once before the loop, use a join, or batch the keys (e.g. chunkBySize)."
                s.SourceText)
            s.Range
            [])

[<EditorAnalyzer("QueryInLoop", "Note IQueryable iteration nested in another loop (N+1)", HelpBase)>]
let queryInLoopEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0028" "QueryInLoop" (fun () ->
        whenChecked ctx (queryInLoopMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("QueryInLoop", "Note IQueryable iteration nested in another loop (N+1)", HelpBase)>]
let queryInLoopCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0028" "QueryInLoop" (fun () ->
        queryInLoopMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0029 TaskStateMachine ----

let private taskStateMachineMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    TaskStateMachine.find parseTree source
    |> List.map (fun s ->
        let message =
            match s.Kind with
            | TaskStateMachine.AdviceKind.HoistRecursiveFunction ->
                "A let rec inside task { } cannot be compiled into the static state machine (FS3511 at build time); move the recursive function out of the task."
            | TaskStateMachine.AdviceKind.HoistPlainLets count ->
                sprintf
                    "This task is large enough to risk the dynamic state-machine fallback (FS3511): %d plain let binding(s) before the first await can move out of the task."
                    count
            | TaskStateMachine.AdviceKind.SplitBranches ->
                "This task is large enough to risk the dynamic state-machine fallback (FS3511): several branches await work, and each branch can become its own smaller task { }."
            | TaskStateMachine.AdviceKind.ExtractTail lines ->
                sprintf
                    "This task is large enough to risk the dynamic state-machine fallback (FS3511): %d lines of non-awaiting code follow the last await and can extract into a plain function."
                    lines

        hint "FR0029" message s.Range [])

[<EditorAnalyzer("TaskStateMachine", "Advice for shrinking oversized task expressions", HelpBase)>]
let taskStateMachineEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0029" "TaskStateMachine" (fun () ->
        taskStateMachineMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("TaskStateMachine", "Advice for shrinking oversized task expressions", HelpBase)>]
let taskStateMachineCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0029" "TaskStateMachine" (fun () ->
        taskStateMachineMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0030 AddRange ----

let private addRangeMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    AddRange.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0030"
            "This loop only accumulates into a ResizeArray; a single AddRange call does the same (and pre-sizes when the source count is known)."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("AddRange", "Collapse accumulate-only loops to ResizeArray.AddRange", HelpBase)>]
let addRangeEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0030" "AddRange" (fun () ->
        whenChecked ctx (addRangeMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("AddRange", "Collapse accumulate-only loops to ResizeArray.AddRange", HelpBase)>]
let addRangeCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0030" "AddRange" (fun () ->
        addRangeMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0031 StringConcat ----

let private stringConcatMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    StringConcat.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0031"
            "This string concatenation chain can be an interpolated string."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("StringConcat", "Rewrite string + chains as interpolated strings", HelpBase)>]
let stringConcatEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0031" "StringConcat" (fun () ->
        whenChecked ctx (stringConcatMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("StringConcat", "Rewrite string + chains as interpolated strings", HelpBase)>]
let stringConcatCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0031" "StringConcat" (fun () ->
        stringConcatMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0032 / FR0033 ObjectDesign ----

let private objectDesignMessages
    (fileName: string)
    (parseTree: ParsedInput)
    (source: ISourceText)
    checkResults
    : Message list =
    let disposableEnabled =
        Configuration.isRuleEnabled fileName "FR0032" "DisposableField"

    let staticEnabled = Configuration.isRuleEnabled fileName "FR0033" "StaticMember"

    if not (disposableEnabled || staticEnabled) then
        []
    else
        let disposables, statics = ObjectDesign.find parseTree source checkResults

        let disposableMessages =
            if disposableEnabled then
                disposables
                |> List.map (fun s ->
                    hint
                        "FR0032"
                        (sprintf
                            "Type '%s' creates disposable '%s' but does not implement IDisposable; the resource has no owner to dispose it."
                            s.TypeName
                            s.FieldName)
                        s.Range
                        [])
            else
                []

        let staticMessages =
            if staticEnabled then
                statics
                |> List.map (fun s ->
                    hint
                        "FR0033"
                        (sprintf
                            "Member '%s' uses no instance state and can be a static member (call sites change from instance to type)."
                            s.MemberName)
                        s.Range
                        [])
            else
                []

        disposableMessages @ staticMessages

[<EditorAnalyzer("ObjectDesign", "Disposable fields without IDisposable; could-be-static members", HelpBase)>]
let objectDesignEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return whenChecked ctx (objectDesignMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText) }

[<CliAnalyzer("ObjectDesign", "Disposable fields without IDisposable; could-be-static members", HelpBase)>]
let objectDesignCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async {
        return objectDesignMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults
    }

// ---- FR0034 OptionMatch ----

let private optionMatchMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    OptionMatch.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0034"
            "IsSome test plus .Value access can be a pattern match; .Value throws when the option is empty, the match cannot."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("OptionMatch", "Rewrite IsSome/.Value conditionals as pattern matches", HelpBase)>]
let optionMatchEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0034" "OptionMatch" (fun () ->
        whenChecked ctx (optionMatchMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("OptionMatch", "Rewrite IsSome/.Value conditionals as pattern matches", HelpBase)>]
let optionMatchCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0034" "OptionMatch" (fun () ->
        optionMatchMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0035 / FR0037 LoopPerf ----

let private loopPerfMessages (fileName: string) (parseTree: ParsedInput) (source: ISourceText) : Message list =
    let containsEnabled = Configuration.isRuleEnabled fileName "FR0035" "ContainsInLoop"

    let constructionEnabled =
        Configuration.isRuleEnabled fileName "FR0037" "ConstructionInLoop"

    if not (containsEnabled || constructionEnabled) then
        []
    else
        let contains, constructions = LoopPerf.find parseTree source

        let containsMessages =
            if containsEnabled then
                contains
                |> List.map (fun s ->
                    hint
                        "FR0035"
                        (sprintf
                            "%s.contains scans '%s' linearly on every iteration; build a Set from it once outside the loop for O(log n) probes."
                            s.ModuleName
                            s.CollectionName)
                        s.Range
                        [])
            else
                []

        let constructionMessages =
            if constructionEnabled then
                constructions
                |> List.map (fun s ->
                    hint
                        "FR0037"
                        (sprintf
                            "A %s is constructed on every iteration; it is expensive by design — hoist it outside the loop or make it static."
                            s.TypeName)
                        s.Range
                        [])
            else
                []

        containsMessages @ constructionMessages

[<EditorAnalyzer("LoopPerf", "Linear probes and expensive constructions inside loops", HelpBase)>]
let loopPerfEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return loopPerfMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText }

[<CliAnalyzer("LoopPerf", "Linear probes and expensive constructions inside loops", HelpBase)>]
let loopPerfCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return loopPerfMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText }

// ---- FR0036 TypeChecks ----

let private typeChecksMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    TypeChecks.find parseTree source
    |> List.map (fun s ->
        let message =
            match s.Kind with
            | TypeChecks.TypeCheckKind.NameComparison prop ->
                sprintf
                    "Comparing GetType().%s to a string breaks silently on renames and namespaces; compare types instead (a ':?' type test or typeof<_> equality)."
                    prop
            | TypeChecks.TypeCheckKind.TypeofEquality(receiver, typeName) ->
                sprintf
                    "GetType() = typeof<%s> is exact-type equality; if matching subtypes is fine (it usually is), '%s :? %s' says it directly."
                    typeName
                    receiver
                    typeName

        hint "FR0036" message s.Range [])

[<EditorAnalyzer("TypeChecks", "Fragile runtime type comparisons", HelpBase)>]
let typeChecksEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0036" "TypeChecks" (fun () ->
        typeChecksMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("TypeChecks", "Fragile runtime type comparisons", HelpBase)>]
let typeChecksCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0036" "TypeChecks" (fun () ->
        typeChecksMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0038 CharOverload ----

let private charOverloadMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    CharOverload.find parseTree source checkResults
    |> List.map (fun s ->
        match s.ReplacementText with
        | Some replacement ->
            hint
                "FR0038"
                (sprintf
                    "%s has a char overload for a single character; it skips the string-comparison setup."
                    s.MethodName)
                s.Range
                [ fix s.Range s.OriginalText replacement ]
        | None ->
            hint
                "FR0038"
                (sprintf
                    "%s with a single-character string has a faster char overload — but the char overload compares ordinally while the string overload is culture-sensitive; switch (or add StringComparison.Ordinal) only if ordinal is intended."
                    s.MethodName)
                s.Range
                [])

[<EditorAnalyzer("CharOverload", "Use char overloads for single-character strings", HelpBase)>]
let charOverloadEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0038" "CharOverload" (fun () ->
        whenChecked ctx (charOverloadMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("CharOverload", "Use char overloads for single-character strings", HelpBase)>]
let charOverloadCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0038" "CharOverload" (fun () ->
        charOverloadMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0039 CaseInsensitive ----

let private caseInsensitiveMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    CaseInsensitive.find parseTree source checkResults
    |> List.map (fun s ->
        let message =
            match s.Kind with
            | CaseInsensitive.CaseKind.Equality ->
                sprintf
                    "%s() allocates a copy just to compare; String.Equals(a, b, StringComparison...IgnoreCase) is allocation-free — pick the comparison type deliberately (Ordinal vs Culture)."
                    s.LoweringName
            | CaseInsensitive.CaseKind.MethodCall method ->
                sprintf
                    "%s() allocates a copy just to call %s; the %s overload taking a StringComparison is allocation-free — pick the comparison type deliberately (Ordinal vs Culture)."
                    s.LoweringName
                    method
                    method

        hint "FR0039" message s.Range [])

[<EditorAnalyzer("CaseInsensitive", "Allocation-free case-insensitive comparisons", HelpBase)>]
let caseInsensitiveEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0039" "CaseInsensitive" (fun () ->
        whenChecked ctx (caseInsensitiveMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("CaseInsensitive", "Allocation-free case-insensitive comparisons", HelpBase)>]
let caseInsensitiveCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0039" "CaseInsensitive" (fun () ->
        caseInsensitiveMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0040 RedundantGuard ----

let private redundantGuardMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    RedundantGuard.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0040"
            (sprintf
                "%s already handles the miss (it returns false); the %s guard just doubles the lookup."
                s.ActionName
                s.GuardName)
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("RedundantGuard", "Drop membership guards before miss-tolerant Remove/Add", HelpBase)>]
let redundantGuardEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0040" "RedundantGuard" (fun () ->
        whenChecked ctx (redundantGuardMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("RedundantGuard", "Drop membership guards before miss-tolerant Remove/Add", HelpBase)>]
let redundantGuardCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0040" "RedundantGuard" (fun () ->
        redundantGuardMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0041 VectorizedLinq ----

let private vectorizedLinqMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    VectorizedLinq.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0041"
            (sprintf
                "Array.%s is a scalar loop; on .NET 8+ System.Linq's %s%s() is SIMD-vectorized for '%s''s element type (note: LINQ Sum throws on overflow where Array.sum wraps)."
                s.FunctionName
                (string (System.Char.ToUpperInvariant s.FunctionName.[0]))
                (s.FunctionName.Substring 1)
                s.ArrayName)
            s.Range
            [])

[<EditorAnalyzer("VectorizedLinq", "SIMD-vectorized LINQ aggregations for primitive arrays", HelpBase)>]
let vectorizedLinqEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0041" "VectorizedLinq" (fun () ->
        whenChecked ctx (vectorizedLinqMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("VectorizedLinq", "SIMD-vectorized LINQ aggregations for primitive arrays", HelpBase)>]
let vectorizedLinqCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0041" "VectorizedLinq" (fun () ->
        vectorizedLinqMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0042 SprintfInterpolation ----

let private sprintfInterpolationMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    SprintfInterpolation.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0042"
            "This sprintf can be a typed interpolated string; the specifiers stay, so the output is identical and the arguments read in place."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("SprintfInterpolation", "Rewrite fully applied sprintf as typed interpolation", HelpBase)>]
let sprintfInterpolationEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0042" "SprintfInterpolation" (fun () ->
        sprintfInterpolationMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("SprintfInterpolation", "Rewrite fully applied sprintf as typed interpolation", HelpBase)>]
let sprintfInterpolationCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0042" "SprintfInterpolation" (fun () ->
        sprintfInterpolationMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0043 TypedHoles ----

let private typedHolesMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    TypedHoles.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0043"
            (sprintf
                "This string already uses typed holes; '%s{%s}' pins the type at compile time with identical output."
                s.Specifier
                s.FillText)
            s.Range
            [ fix s.Range "" s.Specifier ])

[<EditorAnalyzer("TypedHoles", "Type the remaining holes of already-typed interpolations", HelpBase)>]
let typedHolesEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0043" "TypedHoles" (fun () ->
        whenChecked ctx (typedHolesMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("TypedHoles", "Type the remaining holes of already-typed interpolations", HelpBase)>]
let typedHolesCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0043" "TypedHoles" (fun () ->
        typedHolesMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0021 InterpToString ----

let private interpToStringMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    InterpToString.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0021"
            "Redundant ToString() inside an interpolated string; interpolation formats the value already."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("InterpToString", "Drop redundant ToString() in interpolated strings", HelpBase)>]
let interpToStringEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0021" "InterpToString" (fun () ->
        interpToStringMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("InterpToString", "Drop redundant ToString() in interpolated strings", HelpBase)>]
let interpToStringCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0021" "InterpToString" (fun () ->
        interpToStringMessages ctx.ParseFileResults.ParseTree ctx.SourceText)
