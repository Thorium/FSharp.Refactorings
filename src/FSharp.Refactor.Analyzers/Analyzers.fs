/// FSharp.Analyzers.SDK entry points. Each refactoring is exposed twice:
/// once for editors (FsAutoComplete/Ionide) and once for the CLI
/// (fsharp-analyzers tool, usable in CI). The logic itself lives in the
/// per-refactoring modules; this file only builds the diagnostic messages
/// and applies the optional per-repository configuration
/// (fsharprefactor.json), which can disable rules by code or name.
module FSharp.Refactor.Analyzers

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK

[<Literal>]
let private HelpBase = "https://github.com/Thorium/fsharp-refactor"

let private fix (range: range) (original: string) (replacement: string) : Fix =
    { FromRange = range
      FromText = original
      ToText = replacement }

/// Every rule's message ends with its kind, so the one place a reader meets a
/// suggestion — an editor hover, a SARIF entry, a CI log — says whether it is
/// a defect or a matter of punctuation, without looking the code up. It is a
/// suffix rather than a prefix because editors truncate from the right, and
/// the sentence matters more than the label.
let private hint (code: string) (message: string) (range: range) (fixes: Fix list) : Message =
    { Type = "FSharp.Refactor"
      Message = $"{message} [{RuleCatalog.name (RuleCatalog.categoryOf code)}]"
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

/// The EDITOR-side twin of the apply tool's comment guard: a fix whose
/// span contains a comment that no fix of the same message re-emits would
/// silently DELETE it through the light bulb — and unlike the CLI, the
/// editor has no build check or hold-back behind it. Messages carrying
/// such fixes are dropped from editor results entirely; the CLI keeps its
/// own guard, which also REPORTS the hold-back. Applied by the editor
/// wrappers of every rule whose fixes can span multiple lines.
let commentSafeOnly (parseTree: ParsedInput) (source: ISourceText) (messages: Message list) : Message list =
    match messages |> List.filter (fun m -> not m.Fixes.IsEmpty) with
    | [] -> messages
    | _ ->
        let comments = Text.commentsWithText parseTree source

        if comments.IsEmpty then
            messages
        else
            messages
            |> List.filter (fun m ->
                m.Fixes.IsEmpty
                || (let toTexts = m.Fixes |> List.map (fun f -> f.ToText)

                    m.Fixes
                    |> List.forall (fun f ->
                        comments
                        |> List.forall (fun (r, text) ->
                            not (Range.rangeContainsRange f.FromRange r)
                            || toTexts |> List.exists (fun t -> t.Contains text)))))

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
        matchToIfMessages ctx.ParseFileResults.ParseTree ctx.SourceText
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("MatchToIf", "Rewrite a boolean match expression as if-else", HelpBase)>]
let matchToIfCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0001" "MatchToIf" (fun () ->
        matchToIfMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0108 / FR0109 BooleanSimplify ----

let private booleanSimplifyMessages (fileName: string) (parseTree: ParsedInput) (source: ISourceText) : Message list =
    let identityEnabled =
        Configuration.isRuleEnabled fileName "FR0108" "BooleanIdentity"

    let duplicateEnabled =
        Configuration.isRuleEnabled fileName "FR0109" "BooleanDuplicate"

    if not (identityEnabled || duplicateEnabled) then
        []
    else
        BooleanSimplify.find parseTree source
        |> List.choose (fun s ->
            match s.Kind with
            | BooleanSimplify.Kind.Identity when identityEnabled ->
                Some(
                    hint
                        "FR0108"
                        "The boolean literal contributes nothing here; the expression is the other operand."
                        s.Range
                        [ fix s.Range s.OriginalText s.ReplacementText ]
                )
            | BooleanSimplify.Kind.Duplicate when duplicateEnabled ->
                Some(
                    hint
                        "FR0109"
                        "Both operands are the same expression; one suffices — unless the duplicate was meant to be something else, which is worth a look."
                        s.Range
                        [ fix s.Range s.OriginalText s.ReplacementText ]
                )
            | _ -> None)

[<EditorAnalyzer("BooleanSimplify", "Drop boolean identity literals and duplicated operands", HelpBase)>]
let booleanSimplifyEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return booleanSimplifyMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText }

[<CliAnalyzer("BooleanSimplify", "Drop boolean identity literals and duplicated operands", HelpBase)>]
let booleanSimplifyCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return booleanSimplifyMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText }

// ---- FR0110 MissingCases ----

let private missingCasesMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    MissingCases.find parseTree source checkResults
    |> List.map (fun s ->
        let names = s.MissingCases |> String.concat ", "

        hint
            "FR0110"
            $"This match has no arm for {names} and no wildcard (FS0025); the fix adds the missing arm(s) raising NotImplementedException, so the gap reports itself."
            s.Range
            [ fix s.Range "" s.InsertText ])

[<EditorAnalyzer("MissingCases", "Complete an incomplete DU match with explicit raising arms", HelpBase)>]
let missingCasesEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0110" "MissingCases" (fun () ->
        whenChecked ctx (missingCasesMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("MissingCases", "Complete an incomplete DU match with explicit raising arms", HelpBase)>]
let missingCasesCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0110" "MissingCases" (fun () ->
        missingCasesMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0118 CancellationOverload ----

let private cancellationMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    CancellationOverload.find parseTree source checkResults
    |> List.map (fun s ->
        let message =
            match s.Kind with
            | CancellationOverload.TokenGap.Omitted ->
                $"'{s.MethodName}' has an overload accepting a CancellationToken and '{s.TokenName}' sits unused in scope; without it, cancellation stops propagating exactly one call too early."
            | CancellationOverload.TokenGap.NonePassed ->
                $"CancellationToken.None is passed although '{s.TokenName}' is in scope; the chain is cut here instead of propagated."

        hint "FR0118" message s.Range [ fix s.Range s.Original s.Replacement ])

[<EditorAnalyzer("CancellationOverload", "Pass the in-scope CancellationToken to calls that take one", HelpBase)>]
let cancellationEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0118" "CancellationOverload" (fun () ->
        whenChecked ctx (cancellationMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("CancellationOverload", "Pass the in-scope CancellationToken to calls that take one", HelpBase)>]
let cancellationCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0118" "CancellationOverload" (fun () ->
        cancellationMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0119 AwaitableOverload ----

let private awaitableMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    AwaitableOverload.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0119"
            $"'{s.MethodName}' blocks inside the computation although '{s.MethodName}Async' exists; binding the async twin keeps the thread free — and FR0118 hands it the CancellationToken on the next pass."
            s.Range
            (s.Fixes |> List.map (fun (r, original, replacement) -> fix r original replacement)))

[<EditorAnalyzer("AwaitableOverload", "Use the async twin of a blocking call inside task/async", HelpBase)>]
let awaitableEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0119" "AwaitableOverload" (fun () ->
        whenChecked ctx (awaitableMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("AwaitableOverload", "Use the async twin of a blocking call inside task/async", HelpBase)>]
let awaitableCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0119" "AwaitableOverload" (fun () ->
        awaitableMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0120 CatchLogException ----

let private catchLogMessages
    (parseTree: ParsedInput)
    (source: ISourceText)
    (offerAlternatives: bool)
    checkResults
    : Message list =
    CatchLogException.find parseTree source checkResults
    |> List.collect (fun s ->
        let primary =
            hint
                "FR0120"
                $"This {s.LogMethod} inside the handler never mentions '{s.ExceptionName}' — the one fact the handler exists to record; passing it first lets the sink decide rendering (logging only {s.ExceptionName}.Message deliberately is a legitimate PII choice — write that instead)."
                s.Range
                [ fix s.Range "" $"{s.ExceptionName}, " ]

        if offerAlternatives then
            [ primary
              hint
                  "FR0120"
                  $"Alternative: pass {s.ExceptionName}.GetBaseException() — the root cause of a wrapped or aggregate exception."
                  s.Range
                  [ fix s.Range "" $"{s.ExceptionName}.GetBaseException(), " ] ]
        else
            [ primary ])

[<EditorAnalyzer("CatchLogException", "Pass the caught exception to catch-clause log calls", HelpBase)>]
let catchLogEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0120" "CatchLogException" (fun () ->
        whenChecked ctx (catchLogMessages ctx.ParseFileResults.ParseTree ctx.SourceText true))

[<CliAnalyzer("CatchLogException", "Pass the caught exception to catch-clause log calls", HelpBase)>]
let catchLogCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0120" "CatchLogException" (fun () ->
        catchLogMessages ctx.ParseFileResults.ParseTree ctx.SourceText false ctx.CheckFileResults)

// ---- FR0121 DateTimeRules ----

let private dateTimeMessages
    (fileName: string)
    (parseTree: ParsedInput)
    (source: ISourceText)
    (offerNowFix: bool)
    checkResults
    : Message list =
    DateTimeRules.find parseTree source checkResults
    |> List.map (fun s ->
        match s.Kind, s.FixRange with
        | DateTimeRules.WallClockKind.UtcDateCut text, _ ->
            hint
                "FR0121"
                $"'{text}' cuts a calendar date at a timezone-random instant — UTC midnight is nobody's midnight, and the server's own date is a deployment accident the end user never sees; convert to the USER'S timezone first, then take the date."
                s.Range
                []
        | DateTimeRules.WallClockKind.LocalNow, Some fixRange when
            offerNowFix
            || Configuration.parameterInt fileName "FR0121" "DateTimeRules" "utcNow" 0 = 1
            ->
            hint
                "FR0121"
                "DateTime.Now reads the server's local clock — a deployment accident; DateTime.UtcNow records an instant. (Local time is right for Fable/desktop code: leave this off there.)"
                s.Range
                [ fix fixRange "Now" "UtcNow" ]
        | DateTimeRules.WallClockKind.LocalNow, _ ->
            hint
                "FR0121"
                "DateTime.Now reads the server's local clock — a deployment accident; DateTime.UtcNow records an instant. Opt the rewrite in with { \"FR0121\": { \"utcNow\": 1 } } (server code), or ignore for Fable/desktop."
                s.Range
                [])

[<EditorAnalyzer("DateTimeRules", "Timezone-random date cuts; opt-in UtcNow rewrite", HelpBase)>]
let dateTimeEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0121" "DateTimeRules" (fun () ->
        whenChecked ctx (dateTimeMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText true))

[<CliAnalyzer("DateTimeRules", "Timezone-random date cuts; opt-in UtcNow rewrite", HelpBase)>]
let dateTimeCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0121" "DateTimeRules" (fun () ->
        dateTimeMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText false ctx.CheckFileResults)

// ---- FR0123 MonitorLock ----

let private monitorLockMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    MonitorLock.find parseTree source checkResults
    |> List.map (fun s ->
        match s.Fix with
        | Some(r, original, replacement) ->
            hint
                "FR0123"
                $"Monitor.Enter/try/finally/Monitor.Exit over '{s.LockText}' is the `lock` function spelled dangerously; `lock` releases on every path by construction."
                s.Range
                [ fix r original replacement ]
        | None ->
            hint
                "FR0123"
                $"Monitor.Enter '{s.LockText}' without a guarding try/finally leaks the lock on the first exception; `lock {s.LockText} (fun () -> ...)` cannot."
                s.Range
                [])

[<EditorAnalyzer("MonitorLock", "Monitor.Enter/Exit pairs become the lock function", HelpBase)>]
let monitorLockEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0123" "MonitorLock" (fun () ->
        whenChecked ctx (monitorLockMessages ctx.ParseFileResults.ParseTree ctx.SourceText)
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("MonitorLock", "Monitor.Enter/Exit pairs become the lock function", HelpBase)>]
let monitorLockCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0123" "MonitorLock" (fun () ->
        monitorLockMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0124 LogTemplates ----

let private logTemplateMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    LogTemplates.find parseTree source checkResults
    |> List.map (fun s ->
        let message =
            match s.Problem with
            | LogTemplates.TemplateProblem.CountMismatch(placeholders, arguments) ->
                $"This {s.LogMethod} template names {placeholders} placeholder(s) but receives {arguments} argument(s); the sink logs holes or drops values silently."
            | LogTemplates.TemplateProblem.DuplicateName name ->
                $"This {s.LogMethod} template names '{{{name}}}' twice; structured sinks key properties by name, so one value overwrites the other."
            | LogTemplates.TemplateProblem.Interpolated ->
                $"An interpolated string as a {s.LogMethod} template destroys structured logging: every message becomes a distinct event, and the values lose their property names — use a constant template with placeholders."

        hint "FR0124" message s.Range [])

[<EditorAnalyzer("LogTemplates", "Structured-log templates must match their arguments", HelpBase)>]
let logTemplatesEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0124" "LogTemplates" (fun () ->
        whenChecked ctx (logTemplateMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("LogTemplates", "Structured-log templates must match their arguments", HelpBase)>]
let logTemplatesCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0124" "LogTemplates" (fun () ->
        logTemplateMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0117 MatchArmMerge ----

let private matchArmMergeMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    MissingCases.findMergeableArms parseTree source
    |> List.map (fun s ->
        hint
            "FR0117"
            $"{s.Count} adjacent arms return the same result; one or-pattern arm says it once — same patterns, same order."
            s.ReplaceRange
            [ fix s.ReplaceRange (Text.textOfRange source s.ReplaceRange) s.NewText ])

[<EditorAnalyzer("MatchArmMerge", "Fold adjacent same-result match arms into an or-pattern", HelpBase)>]
let matchArmMergeEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0117" "MatchArmMerge" (fun () ->
        matchArmMergeMessages ctx.ParseFileResults.ParseTree ctx.SourceText
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("MatchArmMerge", "Fold adjacent same-result match arms into an or-pattern", HelpBase)>]
let matchArmMergeCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0117" "MatchArmMerge" (fun () ->
        matchArmMergeMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0111 / FR0112 / FR0113 IfRestructure ----

let private ifRestructureMessages
    (fileName: string)
    (parseTree: ParsedInput)
    (source: ISourceText)
    checkResults
    : Message list =
    let elseIfEnabled = Configuration.isRuleEnabled fileName "FR0111" "ElseIfFlatten"

    let chainEnabled =
        Configuration.isRuleEnabled fileName "FR0112" "EqualityChainToMatch"

    let mergeEnabled = Configuration.isRuleEnabled fileName "FR0113" "NestedIfMerge"

    let flipEnabled = Configuration.isRuleEnabled fileName "FR0114" "PyramidFlip"

    let guardOrderEnabled = Configuration.isRuleEnabled fileName "FR0115" "GuardOrder"

    // configurable knobs, FR0114's per-rule parameters:
    //     { "FR0114": { "enabled": true, "thenAtLeast": 30, "elseAtMost": 2 } }
    let thenAtLeast =
        Configuration.parameterInt fileName "FR0114" "PyramidFlip" "thenAtLeast" 20

    let elseAtMost =
        Configuration.parameterInt fileName "FR0114" "PyramidFlip" "elseAtMost" 3
        // overlapping thresholds would make the flip fire on its own
        // output and oscillate every pass; clamp so a flipped branch can
        // never re-qualify
        |> min (thenAtLeast - 1)

    [ if flipEnabled then
          for s in IfRestructure.findPyramidFlips thenAtLeast elseAtMost parseTree source do
              hint
                  "FR0114"
                  "A large then-branch behind a small else reads bottom-heavy; flipping the condition puts the short exit first."
                  s.Range
                  [ fix s.Range s.OriginalText s.ReplacementText ]
      if guardOrderEnabled then
          for s in IfRestructure.findGuardOrderNotes parseTree source do
              hint
                  "FR0115"
                  $"The base case sits FIRST behind a compound guard on '{s.Variable}'; every new error condition must be threaded into it. Inverted — error guards first, the base case as the final arm — the match reads top-down and extends by appending."
                  s.Range
                  []
      if elseIfEnabled then
          for s in IfRestructure.findElseIf parseTree source do
              hint
                  "FR0111"
                  "This `else` holds a whole nested if; `elif` says the same thing one level flatter."
                  s.Range
                  [ fix s.Range s.OriginalText s.ReplacementText ]
      if chainEnabled then
          for s in IfRestructure.findEqualityChains parseTree source checkResults do
              hint
                  "FR0112"
                  "This if/elif chain compares one identifier against distinct literals; a match states the same dispatch directly."
                  s.Range
                  [ fix s.Range s.OriginalText s.ReplacementText ]
      if mergeEnabled then
          for s in IfRestructure.findNestedIfMerges parseTree source do
              hint
                  "FR0113"
                  "The nested if can merge into one `&&` condition — the branches are unchanged, one level of nesting is gone."
                  s.Range
                  [ fix s.Range s.OriginalText s.ReplacementText ] ]

[<EditorAnalyzer("IfRestructure", "Flatten else-if, chain-to-match, nested-if merges", HelpBase)>]
let ifRestructureEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async {
        return
            whenChecked ctx (ifRestructureMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText)
            |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText
    }

[<CliAnalyzer("IfRestructure", "Flatten else-if, chain-to-match, nested-if merges", HelpBase)>]
let ifRestructureCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async {
        return ifRestructureMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults
    }

// ---- FR0116 RecGroup ----

let private recGroupMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    let extractions =
        RecGroup.find parseTree source
        |> List.map (fun s ->
            let explanation =
                if s.IsSelfRecursive then
                    $"'{s.MemberName}' calls only itself, no other member of its `let rec` group; its own `let rec` above the group narrows the knot."
                else
                    $"'{s.MemberName}' references no member of its `let rec` group; a plain `let` above the group says it takes part in no recursion."

            hint
                "FR0116"
                explanation
                s.RemoveRange
                [ fix s.InsertRange "" s.InsertText
                  fix s.RemoveRange (Text.textOfRange source s.RemoveRange) "" ])

    let recrowns =
        RecGroup.findHeadRecrowns parseTree source
        |> List.map (fun s ->
            hint
                "FR0116"
                $"'{s.MemberName}' heads its `let rec` group but references no member; a plain `let` with the group re-crowned below says it takes part in no recursion."
                s.LetRecRange
                [ fix s.LetRecRange (Text.textOfRange source s.LetRecRange) "let"
                  fix s.AndRange (Text.textOfRange source s.AndRange) "let rec" ])

    extractions @ recrowns

[<EditorAnalyzer("RecGroup", "Pull non-recursive members out of let rec groups", HelpBase)>]
let recGroupEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0116" "RecGroup" (fun () ->
        recGroupMessages ctx.ParseFileResults.ParseTree ctx.SourceText
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("RecGroup", "Pull non-recursive members out of let rec groups", HelpBase)>]
let recGroupCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0116" "RecGroup" (fun () ->
        recGroupMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

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
        whenChecked ctx (optionModuleMessages ctx.ParseFileResults.ParseTree ctx.SourceText)
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

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
        compositionMessages ctx.ParseFileResults.ParseTree ctx.SourceText
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

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
        conversionMoveMessages ctx.ParseFileResults.ParseTree ctx.SourceText
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

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
            | CeStrip.StripKind.ReturnBangIdentity ->
                "return! around a builder whose whole body is one return statement is a no-op machine; the inner statement is the arm."
            | CeStrip.StripKind.TaskFromResult ->
                "This task wrapping only wraps a value and can be written with Task.FromResult."
            | CeStrip.StripKind.ThunkIdentity ->
                "This tail thunk is defined and immediately called; the binding is its body."

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

let private activePatternMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    ActivePattern.find parseTree source checkResults
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
        whenChecked ctx (activePatternMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("ActivePattern", "Extract a when-guard into an active pattern", HelpBase)>]
let activePatternCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0006" "ActivePattern" (fun () ->
        activePatternMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

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
        whenChecked ctx (resultModuleMessages ctx.ParseFileResults.ParseTree ctx.SourceText)
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

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
    StructActivePattern.find (Visibility.apiChangesAllowed ()) parseTree source checkResults
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

let private hintMessages
    (extraRules: string list)
    (parseTree: ParsedInput)
    (source: ISourceText)
    (check: FSharpCheckFileResults option)
    : Message list =
    HintEngine.find extraRules parseTree source check
    |> List.map (fun s ->
        hint
            "FR0012"
            ($"This expression can be simplified (%s{s.Rule}).")
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("Hints", "Term-rewriting hints (fsharplint-style rules)", HelpBase)>]
let hintsEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0012" "Hints" (fun () ->
        hintMessages
            (Configuration.hintsFor ctx.FileName)
            ctx.ParseFileResults.ParseTree
            ctx.SourceText
            ctx.CheckFileResults)

[<CliAnalyzer("Hints", "Term-rewriting hints (fsharplint-style rules)", HelpBase)>]
let hintsCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0012" "Hints" (fun () ->
        hintMessages
            (Configuration.hintsFor ctx.FileName)
            ctx.ParseFileResults.ParseTree
            ctx.SourceText
            (Some ctx.CheckFileResults))

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

// ---- FR0094 MethodCallParens ----

let private methodCallParensMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    MethodCallParens.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0094"
            "Redundant parentheses around a single atomic method-call argument."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("MethodCallParens", "Drop redundant parentheses around single method-call arguments", HelpBase)>]
let methodCallParensEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0094" "MethodCallParens" (fun () ->
        methodCallParensMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("MethodCallParens", "Drop redundant parentheses around single method-call arguments", HelpBase)>]
let methodCallParensCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0094" "MethodCallParens" (fun () ->
        methodCallParensMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0095 LambdaBuiltin ----

let private lambdaBuiltinMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    LambdaBuiltin.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0095"
            $"This lambda is exactly `{s.ReplacementText}`."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("LambdaBuiltin", "Lambdas that restate id, fst or snd", HelpBase)>]
let lambdaBuiltinEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0095" "LambdaBuiltin" (fun () ->
        lambdaBuiltinMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("LambdaBuiltin", "Lambdas that restate id, fst or snd", HelpBase)>]
let lambdaBuiltinCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0095" "LambdaBuiltin" (fun () ->
        lambdaBuiltinMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0096 PatternParens ----

let private patternParensMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    PatternParens.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0096"
            "Redundant parentheses around a pattern."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("PatternParens", "Drop redundant parentheses around patterns", HelpBase)>]
let patternParensEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0096" "PatternParens" (fun () ->
        patternParensMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("PatternParens", "Drop redundant parentheses around patterns", HelpBase)>]
let patternParensCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0096" "PatternParens" (fun () ->
        patternParensMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0097 / FR0098 TypeSyntax ----

let private typeParensMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    TypeSyntax.findRedundantParens parseTree source
    |> List.map (fun s ->
        hint "FR0097" "Redundant parentheses around a type." s.Range [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("TypeParens", "Drop redundant parentheses around types", HelpBase)>]
let typeParensEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0097" "TypeParens" (fun () ->
        typeParensMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("TypeParens", "Drop redundant parentheses around types", HelpBase)>]
let typeParensCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0097" "TypeParens" (fun () ->
        typeParensMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

let private abbreviatedTypeMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    TypeSyntax.findAbbreviations parseTree source
    |> List.map (fun s ->
        hint
            "FR0098"
            $"F# abbreviates this type as `{s.ReplacementText}`."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("AbbreviatedType", "Use F# type abbreviations for BCL names", HelpBase)>]
let abbreviatedTypeEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0098" "AbbreviatedType" (fun () ->
        abbreviatedTypeMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("AbbreviatedType", "Use F# type abbreviations for BCL names", HelpBase)>]
let abbreviatedTypeCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0098" "AbbreviatedType" (fun () ->
        abbreviatedTypeMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0099 TrailingSemicolon ----

let private trailingSemicolonMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    TrailingSemicolon.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0099"
            "A `;` at the end of a line does nothing in light syntax."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("TrailingSemicolon", "Drop line-ending semicolons", HelpBase)>]
let trailingSemicolonEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0099" "TrailingSemicolon" (fun () ->
        trailingSemicolonMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("TrailingSemicolon", "Drop line-ending semicolons", HelpBase)>]
let trailingSemicolonCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0099" "TrailingSemicolon" (fun () ->
        trailingSemicolonMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0100 UnimplementedBranch ----

let private unimplementedBranchMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    UnimplementedBranch.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0100"
            "This branch says it is unfinished and then returns a value a caller cannot tell from a real one; `raise (NotImplementedException())` reports the gap where it is."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("UnimplementedBranch", "Unfinished match branches returning a stand-in value", HelpBase)>]
let unimplementedBranchEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0100" "UnimplementedBranch" (fun () ->
        unimplementedBranchMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("UnimplementedBranch", "Unfinished match branches returning a stand-in value", HelpBase)>]
let unimplementedBranchCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0100" "UnimplementedBranch" (fun () ->
        unimplementedBranchMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

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
        whenChecked ctx (dictTryGetMessages ctx.ParseFileResults.ParseTree ctx.SourceText)
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

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

// ---- FR0122 RegexValidity ----

let private regexValidityMessages (parseTree: ParsedInput) : Message list =
    RegexUsage.findInvalidPatterns parseTree
    |> List.map (fun (r, pattern, error) ->
        let firstLine =
            match error.IndexOf '\n' with
            | -1 -> error
            | cut -> error.Substring(0, cut).TrimEnd()

        hint "FR0122" $"This regex pattern does not compile — a guaranteed ArgumentException on first use: {firstLine}" r [])

[<EditorAnalyzer("RegexValidity", "Literal regex patterns must compile", HelpBase)>]
let regexValidityEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0122" "RegexValidity" (fun () -> regexValidityMessages ctx.ParseFileResults.ParseTree)

[<CliAnalyzer("RegexValidity", "Literal regex patterns must compile", HelpBase)>]
let regexValidityCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0122" "RegexValidity" (fun () -> regexValidityMessages ctx.ParseFileResults.ParseTree)

// ---- FR0016 StructDu ----

let private structDuMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    StructDu.find (Visibility.apiChangesAllowed ()) parseTree source
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
    DuFieldNames.find (Visibility.apiChangesAllowed ()) parseTree source
    |> List.map (fun s ->
        hint
            "FR0022"
            (sprintf
                "Union case '%s' can name its fields (%s) after the names %s already spell."
                s.CaseName
                (String.concat ", " s.Names)
                s.Source)
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
        whenChecked ctx (dictTryAddMessages ctx.ParseFileResults.ParseTree ctx.SourceText)
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("DictTryAdd", "Replace check-then-add with TryAdd", HelpBase)>]
let dictTryAddCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0018" "DictTryAdd" (fun () ->
        dictTryAddMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0019 / FR0020 / FR0054 ObjectRules ----

let private objectRulesMessages (fileName: string) (parseTree: ParsedInput) (source: ISourceText) : Message list =
    let equalsEnabled = Configuration.isRuleEnabled fileName "FR0019" "EqualsHashCode"
    let ctorEnabled = Configuration.isRuleEnabled fileName "FR0020" "CtorAbstractCall"

    let raiseEnabled =
        Configuration.isRuleEnabled fileName "FR0054" "RaiseInSpecialMember"

    if not (equalsEnabled || ctorEnabled || raiseEnabled) then
        []
    else
        let equalsSuggestions, ctorSuggestions, raiseSuggestions =
            ObjectRules.find parseTree source

        let equalsMessages =
            if equalsEnabled then
                equalsSuggestions
                |> List.map (fun s ->
                    hint
                        "FR0019"
                        (sprintf
                            "Type '%s' overrides Equals without overriding GetHashCode; hash-based collections will misbehave."
                            s.TypeName)
                        s.Range
                        [])
            else
                []

        let ctorMessages =
            if ctorEnabled then
                ctorSuggestions
                |> List.map (fun s ->
                    hint
                        "FR0020"
                        (sprintf
                            "Abstract member '%s' is used during construction; the override runs before the derived class is initialized."
                            s.MemberName)
                        s.Range
                        [])
            else
                []

        let raiseMessages =
            if raiseEnabled then
                raiseSuggestions
                |> List.map (fun s ->
                    hint
                        "FR0054"
                        (sprintf
                            "Raising from %s surprises its implicit callers (hash containers, debuggers, string formatting, finalization); return a defined value or restructure so the failure surfaces elsewhere."
                            s.MemberName)
                        s.Range
                        [])
            else
                []

        equalsMessages @ ctorMessages @ raiseMessages

[<EditorAnalyzer("ObjectRules",
                 "Equals/GetHashCode pairing, ctor-time abstract calls, raises in special members",
                 HelpBase)>]
let objectRulesEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return objectRulesMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText }

[<CliAnalyzer("ObjectRules", "Equals/GetHashCode pairing, ctor-time abstract calls, raises in special members", HelpBase)>]
let objectRulesCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return objectRulesMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText }

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
        whenChecked ctx (optionOfObjMessages ctx.ParseFileResults.ParseTree ctx.SourceText)
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

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
                "'%s' is an IQueryable iterated inside another loop: each outer iteration executes a separate database query (N+1). Materialize it once before the loop, join both sources in a single query { }, or batch the keys (e.g. chunkBySize ~300 per query)."
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
                    "This task is large enough to risk the dynamic state-machine fallback (FS3511): %d plain let binding(s) before the first await can move out of the task (note: a throw in hoisted code then surfaces at the call instead of faulting the Task)."
                    count
            | TaskStateMachine.AdviceKind.SplitBranches ->
                "This task is large enough to risk the dynamic state-machine fallback (FS3511): each branch can become its own smaller task { } — a branch without awaits becomes a trivially static one."
            | TaskStateMachine.AdviceKind.ExtractTail lines ->
                sprintf
                    "This task is large enough to risk the dynamic state-machine fallback (FS3511): %d lines of non-awaiting code follow the last await and can extract into a plain function."
                    lines
            | TaskStateMachine.AdviceKind.ExtractAwaitingSuffix lines ->
                sprintf
                    "This task is large enough to risk the dynamic state-machine fallback (FS3511): its closing %d lines await in shapes a plain function cannot carry, but they can become their own task-returning function consumed with return! — two smaller state machines instead of one large."
                    lines

        hint "FR0029" message s.Range (s.Edits |> List.map (fun (r, t) -> fix r (Text.textOfRange source r) t)))

[<EditorAnalyzer("TaskStateMachine", "Advice for shrinking oversized task expressions", HelpBase)>]
let taskStateMachineEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0029" "TaskStateMachine" (fun () ->
        taskStateMachineMessages ctx.ParseFileResults.ParseTree ctx.SourceText
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

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

let private stringConcatMessages
    (offerAlternatives: bool)
    (parseTree: ParsedInput)
    (source: ISourceText)
    checkResults
    : Message list =
    StringConcat.find parseTree source checkResults
    |> List.collect (fun s ->
        let primary =
            hint
                "FR0031"
                "This string concatenation chain can be an interpolated string."
                s.Range
                [ fix s.Range s.OriginalText s.ReplacementText ]

        match s.ConcatAlternative with
        | Some concat when offerAlternatives ->
            [ primary
              hint
                  "FR0031"
                  "…or as one explicit String.Concat call — the same thing the compiler emits for the interpolation, spelled out."
                  s.Range
                  [ fix s.Range s.OriginalText concat ] ]
        | _ -> [ primary ])

[<EditorAnalyzer("StringConcat", "Rewrite string + chains as interpolated strings", HelpBase)>]
let stringConcatEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0031" "StringConcat" (fun () ->
        whenChecked ctx (stringConcatMessages true ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("StringConcat", "Rewrite string + chains as interpolated strings", HelpBase)>]
let stringConcatCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0031" "StringConcat" (fun () ->
        stringConcatMessages false ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

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

    let undisposedEnabled =
        Configuration.isRuleEnabled fileName "FR0047" "UndisposedField"

    if not (disposableEnabled || staticEnabled || undisposedEnabled) then
        []
    else
        let disposables, statics, undisposedFields =
            ObjectDesign.find parseTree source checkResults

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

        let undisposedMessages =
            if undisposedEnabled then
                undisposedFields
                |> List.map (fun s ->
                    hint
                        "FR0047"
                        (sprintf
                            "Type '%s' is IDisposable but its Dispose never touches disposable field '%s'; the resource leaks despite the pattern."
                            s.TypeName
                            s.FieldName)
                        s.Range
                        [])
            else
                []

        disposableMessages @ staticMessages @ undisposedMessages

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
        whenChecked ctx (optionMatchMessages ctx.ParseFileResults.ParseTree ctx.SourceText)
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

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
                    if not s.Fix.IsEmpty then
                        hint
                            "FR0035"
                            (sprintf
                                "%s.contains scans '%s' linearly on every iteration; '%s' is a startup-built module binding, so the fix adds a private HashSet companion beside it (built once) and probes that in O(1) — every probe of it in this file converts together."
                                s.ModuleName
                                s.CollectionName
                                s.CollectionName)
                            s.Range
                            (s.Fix |> List.map (fun (r, original, replacement) -> fix r original replacement))
                    else
                        hint
                            "FR0035"
                            (sprintf
                                "%s.contains scans '%s' linearly on every iteration. If the loop is long and '%s' is more than a handful of elements, build a HashSet from it once outside the loop for O(1) probes — the one-time build only pays for itself then; for a few elements the linear scan is already the fastest option, and F# Set's persistent tree costs more to build and probe than HashSet unless you need its immutability."
                                s.ModuleName
                                s.CollectionName
                                s.CollectionName)
                            s.Range
                            [])
            else
                []

        let constructionMessages =
            if constructionEnabled then
                constructions
                |> List.map (fun s ->
                    let message =
                        if s.TypeName = "HttpClient" then
                            // not merely expensive: each instance owns a
                            // socket pool, and per-iteration construction
                            // exhausts ports under load (TIME_WAIT). The
                            // right lifetime is framework-dependent (a
                            // long-lived instance, or IHttpClientFactory
                            // under DI), so this stays advice
                            "An HttpClient is constructed on every iteration — under load this exhausts sockets (TIME_WAIT) and skips DNS refresh. Reuse one long-lived client (it is thread-safe for concurrent requests) or take an IHttpClientFactory."
                        else
                            sprintf
                                "A %s is constructed on every iteration; it is expensive by design — hoist it outside the loop or make it static."
                                s.TypeName

                    hint "FR0037" message s.Range [])
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
                // a capability fix: on a dual-framework run this may emit
                // an #if NET6_0_OR_GREATER / #else pair (char overloads
                // are net-core-era; net4x siblings compile the #else)
                [ CapabilityFix.make source s.Range s.OriginalText replacement ]
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

let private caseInsensitiveMessages
    (offerAlternatives: bool)
    (parseTree: ParsedInput)
    (source: ISourceText)
    checkResults
    : Message list =
    CaseInsensitive.find parseTree source checkResults
    |> List.collect (fun s ->
        let message =
            match s.Kind with
            | CaseInsensitive.CaseKind.Equality when s.Replacement.IsSome ->
                sprintf
                    "%s() allocates a copy just to compare with an ASCII literal; String.Equals(..., OrdinalIgnoreCase) is allocation-free and agrees with it on every input except two Unicode compatibility characters (KELVIN SIGN, LONG S)."
                    s.LoweringName
            | CaseInsensitive.CaseKind.Equality ->
                sprintf
                    "%s() allocates a copy just to compare; String.Equals(a, b, StringComparison...IgnoreCase) is allocation-free — pick the comparison type deliberately (Ordinal vs Culture)."
                    s.LoweringName
            | CaseInsensitive.CaseKind.MethodCall method when s.Replacement.IsSome ->
                sprintf
                    "%s() allocates a copy just to call %s with an ASCII literal; %s(..., OrdinalIgnoreCase) is allocation-free and agrees with it on every input except two Unicode compatibility characters (KELVIN SIGN, LONG S)."
                    s.LoweringName
                    method
                    method
            | CaseInsensitive.CaseKind.MethodCall method ->
                sprintf
                    "%s() allocates a copy just to call %s; the %s overload taking a StringComparison is allocation-free — pick the comparison type deliberately (Ordinal vs Culture)."
                    s.LoweringName
                    method
                    method

        let fixes =
            match s.Replacement with
            | Some replacement -> [ fix s.Range (Text.textOfRange source s.Range) replacement ]
            | None -> []

        let primary = hint "FR0039" message s.Range fixes

        // ALTERNATIVE spellings ride as separate messages so an editor
        // offers each as its own code action; the CLI never sees them and
        // auto-applies only the primary
        match s.CultureReplacement with
        | Some culture when offerAlternatives && s.Replacement.IsSome ->
            [ primary
              hint
                  "FR0039"
                  "…or culture-aware: InvariantCultureIgnoreCase compares by linguistic rules (ligatures, accents) where ordinal compares code points."
                  s.Range
                  [ fix s.Range (Text.textOfRange source s.Range) culture ] ]
        | _ -> [ primary ])

[<EditorAnalyzer("CaseInsensitive", "Allocation-free case-insensitive comparisons", HelpBase)>]
let caseInsensitiveEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0039" "CaseInsensitive" (fun () ->
        whenChecked ctx (caseInsensitiveMessages true ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("CaseInsensitive", "Allocation-free case-insensitive comparisons", HelpBase)>]
let caseInsensitiveCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0039" "CaseInsensitive" (fun () ->
        caseInsensitiveMessages false ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

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
        let message =
            if s.FunctionName = "contains" then
                $"{s.ModuleName}.contains over an array is a scalar loop; on .NET 8+ System.Linq's Contains() is SIMD-vectorized for '{s.ArrayName}''s element type (measured ~5x at 1000 elements, ~6x at 100k)."
            else
                sprintf
                    "%s.%s over an array is a scalar loop; on .NET 8+ System.Linq's %s%s() is SIMD-vectorized for '%s''s element type (note: LINQ Sum throws on overflow where F#'s sum wraps)."
                    s.ModuleName
                    s.FunctionName
                    (string (System.Char.ToUpperInvariant s.FunctionName.[0]))
                    (s.FunctionName.Substring 1)
                    s.ArrayName

        hint "FR0041" message s.Range [])

[<EditorAnalyzer("VectorizedLinq", "SIMD-vectorized LINQ aggregations for primitive arrays", HelpBase)>]
let vectorizedLinqEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0041" "VectorizedLinq" (fun () ->
        whenChecked ctx (vectorizedLinqMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("VectorizedLinq", "SIMD-vectorized LINQ aggregations for primitive arrays", HelpBase)>]
let vectorizedLinqCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0041" "VectorizedLinq" (fun () ->
        vectorizedLinqMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0042 SprintfInterpolation ----

let private sprintfInterpolationMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    SprintfInterpolation.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0042"
            "This sprintf can be a typed interpolated string; the specifiers stay, so the output is identical and the arguments read in place."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("SprintfInterpolation", "Rewrite fully applied sprintf as typed interpolation", HelpBase)>]
let sprintfInterpolationEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0042" "SprintfInterpolation" (fun () ->
        whenChecked ctx (sprintfInterpolationMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("SprintfInterpolation", "Rewrite fully applied sprintf as typed interpolation", HelpBase)>]
let sprintfInterpolationCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0042" "SprintfInterpolation" (fun () ->
        sprintfInterpolationMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

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

// ---- FR0044 Reraise ----

let private reraiseMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    Reraise.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0044"
            (sprintf
                "raise %s resets the exception's stack trace; reraise () rethrows it with the original trace intact."
                s.ExceptionName)
            s.Range
            [ fix s.Range s.OriginalText "reraise ()" ])

[<EditorAnalyzer("Reraise", "Rethrow with reraise () to preserve the stack trace", HelpBase)>]
let reraiseEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0044" "Reraise" (fun () ->
        whenChecked ctx (reraiseMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("Reraise", "Rethrow with reraise () to preserve the stack trace", HelpBase)>]
let reraiseCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0044" "Reraise" (fun () ->
        reraiseMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0045 NaNComparison ----

let private nanComparisonMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    NaNComparison.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0045"
            "Equality against NaN never holds (IEEE 754: NaN is unequal to everything); IsNaN performs the test this comparison meant."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("NaNComparison", "Test NaN with IsNaN, not equality", HelpBase)>]
let nanComparisonEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0045" "NaNComparison" (fun () ->
        whenChecked ctx (nanComparisonMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("NaNComparison", "Test NaN with IsNaN, not equality", HelpBase)>]
let nanComparisonCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0045" "NaNComparison" (fun () ->
        nanComparisonMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0046 WeakLock ----

let private weakLockMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    WeakLock.find parseTree source checkResults
    |> List.map (fun s ->
        let shared =
            match s.Kind with
            | WeakLock.WeakKind.StringValue -> "interned strings are shared process-wide"
            | WeakLock.WeakKind.TypeObject -> "runtime Type objects are shared process-wide"

        hint
            "FR0046"
            (sprintf
                "Locking on %s synchronizes with any code locking the same value (%s); use a dedicated private lock object (let lockObj = obj ())."
                s.TargetText
                shared)
            s.Range
            [])

[<EditorAnalyzer("WeakLock", "Do not lock on strings or Type objects", HelpBase)>]
let weakLockEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0046" "WeakLock" (fun () ->
        whenChecked ctx (weakLockMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("WeakLock", "Do not lock on strings or Type objects", HelpBase)>]
let weakLockCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0046" "WeakLock" (fun () ->
        weakLockMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0048 FormatArgs ----

let private formatArgsMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    FormatArgs.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0048"
            (sprintf
                "The format string references {%d} but only %d argument(s) are supplied; this throws FormatException at runtime (sprintf or interpolation would catch it at compile time)."
                s.MissingIndex
                s.ArgCount)
            s.Range
            [])

[<EditorAnalyzer("FormatArgs", "String.Format placeholders must have arguments", HelpBase)>]
let formatArgsEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0048" "FormatArgs" (fun () ->
        formatArgsMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("FormatArgs", "String.Format placeholders must have arguments", HelpBase)>]
let formatArgsCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0048" "FormatArgs" (fun () ->
        formatArgsMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0049 SyncOverAsync ----

let private syncOverAsyncMessages
    (parseTree: ParsedInput)
    (source: ISourceText)
    (fileName: string)
    (offerSyncSwap: bool)
    checkResults
    : Message list =
    SyncOverAsync.find parseTree source checkResults
    |> List.map (fun s ->
        let message =
            match s.Kind, s.Builder with
            | kind, None ->
                let what =
                    match kind with
                    | SyncOverAsync.BlockKind.TaskResult -> ".Result"
                    | SyncOverAsync.BlockKind.TaskWait -> ".Wait()"
                    | SyncOverAsync.BlockKind.AwaiterGetResult -> "GetAwaiter().GetResult()"
                    | SyncOverAsync.BlockKind.RunSynchronously -> "Async.RunSynchronously"
                    | SyncOverAsync.BlockKind.ThreadSleep -> "Thread.Sleep"

                sprintf
                    "%s is sync-over-async: either make this code async (wrap it in task { } and let!/do!) or call the synchronous API version."
                    what
            | kind, Some builder ->
                let what =
                    match kind with
                    | SyncOverAsync.BlockKind.TaskResult -> ".Result blocks the thread"
                    | SyncOverAsync.BlockKind.TaskWait -> ".Wait() blocks the thread"
                    | SyncOverAsync.BlockKind.AwaiterGetResult -> "GetAwaiter().GetResult() blocks the thread"
                    | SyncOverAsync.BlockKind.RunSynchronously -> "Async.RunSynchronously blocks the thread"
                    | SyncOverAsync.BlockKind.ThreadSleep -> "Thread.Sleep blocks the thread"

                sprintf
                    "%s inside %s { }; bind with let!/do! instead — sync-over-async in a computation expression invites thread-pool starvation and deadlocks."
                    what
                    builder

        // the sync-sibling swap walks code AWAY from async — an editor
        // action the author picks, or a config opt-in for the CLI:
        //     { "FR0049": { "syncSwap": 1 } }
        let swapAllowed =
            offerSyncSwap
            || Configuration.parameterInt fileName "FR0049" "SyncOverAsync" "syncSwap" 0 = 1

        let fixes =
            (s.Fixes @ (if swapAllowed then s.AlternativeFixes else []))
            |> List.map (fun (r, original, replacement) -> fix r original replacement)

        hint "FR0049" message s.Range fixes)

// the taskify fix: a file-private sync function draining a task at its
// boundary becomes task-returning, its callers awaiting — same rule code,
// its own message, all edits in this file
let private taskifyMessages (parseTree: ParsedInput) (source: ISourceText) checkResults projectCheck : Message list =
    Taskify.find parseTree source checkResults projectCheck
    |> List.map (fun s ->
        hint
            "FR0049"
            $"'{s.Name}' drains a task synchronously at its boundary, and every caller already sits in a task/async block; it becomes a task-returning function and the callers await it."
            s.Range
            (s.Edits |> List.map (fun (r, original, replacement) -> fix r original replacement)))

[<EditorAnalyzer("SyncOverAsync", "Blocking waits inside async/task expressions", HelpBase)>]
let syncOverAsyncEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0049" "SyncOverAsync" (fun () ->
        whenChecked ctx (fun check ->
            syncOverAsyncMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.FileName true check
            @ taskifyMessages ctx.ParseFileResults.ParseTree ctx.SourceText check None))

[<CliAnalyzer("SyncOverAsync", "Blocking waits inside async/task expressions", HelpBase)>]
let syncOverAsyncCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0049" "SyncOverAsync" (fun () ->
        syncOverAsyncMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.FileName false ctx.CheckFileResults
        @ taskifyMessages
            ctx.ParseFileResults.ParseTree
            ctx.SourceText
            ctx.CheckFileResults
            (Some ctx.CheckProjectResults))

// ---- FR0050 / FR0051 Accumulation ----

let private accumulationMessages
    (fileName: string)
    (parseTree: ParsedInput)
    (source: ISourceText)
    checkResults
    : Message list =
    let foldEnabled = Configuration.isRuleEnabled fileName "FR0050" "MutableFold"

    let quadraticEnabled =
        Configuration.isRuleEnabled fileName "FR0051" "QuadraticAppend"

    let flagEnabled = Configuration.isRuleEnabled fileName "FR0107" "FlagLoop"

    if not (foldEnabled || quadraticEnabled || flagEnabled) then
        []
    else
        let folds, quadratics = Accumulation.find parseTree source checkResults

        let flagMessages =
            if flagEnabled then
                Accumulation.findFlagLoops parseTree source checkResults
                |> List.map (fun s ->
                    hint
                        "FR0107"
                        "This mutable flag loop asks an exists/forall question; the rewrite answers it directly — and short-circuits, doing the same or less work."
                        s.Range
                        [ fix s.Range s.OriginalText s.ReplacementText ])
            else
                []

        let foldMessages =
            if foldEnabled then
                folds
                |> List.map (fun s ->
                    hint
                        "FR0050"
                        "This mutable accumulator loop is a fold; the rewrite evaluates the same expression with the same bindings, without the mutable."
                        s.Range
                        [ fix s.Range s.OriginalText s.ReplacementText ])
            else
                []

        let quadraticMessages =
            if quadraticEnabled then
                quadratics
                |> List.map (fun s ->
                    let message =
                        match s.Kind with
                        | Accumulation.QuadraticKind.Collection ->
                            sprintf
                                "Appending to '%s' inside a loop copies it every iteration (O(n²)); accumulate into a ResizeArray, or cons with :: and List.rev once at the end."
                                s.Name
                        | Accumulation.QuadraticKind.Str ->
                            sprintf
                                "Building the string '%s' with + inside a loop copies it every iteration (O(n²)) — the slowest way to build a string (measured: 36x slower and 200x the allocation of a StringBuilder at 1000 pieces). Use a StringBuilder, or collect the pieces and String.concat once."
                                s.Name

                    hint "FR0051" message s.Range [])
            else
                []

        foldMessages @ quadraticMessages @ flagMessages

[<EditorAnalyzer("Accumulation", "Mutable accumulator loops and quadratic appends", HelpBase)>]
let accumulationEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async {
        return
            whenChecked ctx (accumulationMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText)
            |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText
    }

[<CliAnalyzer("Accumulation", "Mutable accumulator loops and quadratic appends", HelpBase)>]
let accumulationCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async {
        return accumulationMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults
    }

// ---- FR0052 CountIsEmpty ----

let private countIsEmptyMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    CountIsEmpty.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0052"
            "Count on a concurrent collection walks its segments (O(n) with a snapshot); IsEmpty peeks at the head."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("CountIsEmpty", "Prefer IsEmpty over Count for emptiness checks", HelpBase)>]
let countIsEmptyEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0052" "CountIsEmpty" (fun () ->
        whenChecked ctx (countIsEmptyMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("CountIsEmpty", "Prefer IsEmpty over Count for emptiness checks", HelpBase)>]
let countIsEmptyCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0052" "CountIsEmpty" (fun () ->
        countIsEmptyMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0053 HexString ----

let private hexStringMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    HexString.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0053"
            "BitConverter.ToString + Replace allocates the dashed string just to strip it; Convert.ToHexString produces the identical hex directly."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("HexString", "Hex-encode with Convert.ToHexString", HelpBase)>]
let hexStringEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0053" "HexString" (fun () ->
        whenChecked ctx (hexStringMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("HexString", "Hex-encode with Convert.ToHexString", HelpBase)>]
let hexStringCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0053" "HexString" (fun () ->
        hexStringMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0055 SwallowedException ----

let private swallowedExceptionMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    SwallowedException.find parseTree source
    |> List.map (fun s ->
        let message =
            s.FallbackText
            |> Option.map (fun fallback ->
                $"'with %s{s.PatternText} -> %s{fallback}' swallows every exception and disguises the failure as a legitimate result; log or reraise () — and catch the specific exception type this code can actually handle.")
            |> Option.defaultWith (fun () ->
                $"'with %s{s.PatternText} -> ()' silently swallows every exception, including cancellation and programming errors; log or reraise () — and catch the specific exception type this code can actually handle.")

        hint "FR0055" message s.Range [])

[<EditorAnalyzer("SwallowedException", "Empty catch-all handlers swallow every exception", HelpBase)>]
let swallowedExceptionEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0055" "SwallowedException" (fun () ->
        swallowedExceptionMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("SwallowedException", "Empty catch-all handlers swallow every exception", HelpBase)>]
let swallowedExceptionCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0055" "SwallowedException" (fun () ->
        swallowedExceptionMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0057 XmlDocParams ----

let private xmlDocParamsMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    XmlDocParams.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0057"
            (sprintf
                "The doc comment on '%s' documents some parameters but not %s; drifting docs mislead more than missing ones."
                s.BindingName
                (s.MissingParams |> List.map (sprintf "'%s'") |> String.concat ", "))
            s.Range
            [])

[<EditorAnalyzer("XmlDocParams", "Doc comments that document only some parameters", HelpBase)>]
let xmlDocParamsEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0057" "XmlDocParams" (fun () ->
        xmlDocParamsMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("XmlDocParams", "Doc comments that document only some parameters", HelpBase)>]
let xmlDocParamsCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0057" "XmlDocParams" (fun () ->
        xmlDocParamsMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0058 RecursiveSeq ----

let private recursiveSeqMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    RecursiveSeq.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0058"
            (sprintf
                "'%s' re-enters itself through %s { }: each recursion level allocates a fresh enumerator and every element pays O(depth) MoveNexts. Walk with an explicit Stack/queue inside a single %s { } instead."
                s.FunctionName
                s.Builder
                s.Builder)
            s.Range
            [])

[<EditorAnalyzer("RecursiveSeq", "Recursive re-entry through sequence builders", HelpBase)>]
let recursiveSeqEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0058" "RecursiveSeq" (fun () ->
        recursiveSeqMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("RecursiveSeq", "Recursive re-entry through sequence builders", HelpBase)>]
let recursiveSeqCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0058" "RecursiveSeq" (fun () ->
        recursiveSeqMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0059 StructOption ----

let private structOptionMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    StructOption.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0059"
            (sprintf
                "Private '%s' returns Option, allocating per call; ValueOption is a struct — the definition and every match site are rewritten together."
                s.FunctionName)
            s.DefRange
            (s.Edits
             |> List.map (fun (r, original, replacement) -> fix r original replacement)))

[<EditorAnalyzer("StructOption", "Move private option-returning functions to ValueOption", HelpBase)>]
let structOptionEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0059" "StructOption" (fun () ->
        whenChecked ctx (structOptionMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("StructOption", "Move private option-returning functions to ValueOption", HelpBase)>]
let structOptionCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0059" "StructOption" (fun () ->
        structOptionMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0060 AttributeMerge ----

let private attributeMergeMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    AttributeMerge.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0060"
            "Consecutive attribute brackets can merge into one [<...; ...>] list."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("AttributeMerge", "Merge consecutive attribute brackets", HelpBase)>]
let attributeMergeEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0060" "AttributeMerge" (fun () ->
        attributeMergeMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("AttributeMerge", "Merge consecutive attribute brackets", HelpBase)>]
let attributeMergeCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0060" "AttributeMerge" (fun () ->
        attributeMergeMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0061 ArgNames ----

let private argNamesMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    ArgNames.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0061"
            (sprintf
                "'%s' is not a parameter of this function (parameters: %s); the wrong name sends the caller debugging the wrong argument — nameof would keep it honest."
                s.UsedName
                (String.concat ", " s.ParameterNames))
            s.Range
            [])

[<EditorAnalyzer("ArgNames", "Argument-exception parameter names must exist", HelpBase)>]
let argNamesEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0061" "ArgNames" (fun () ->
        argNamesMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("ArgNames", "Argument-exception parameter names must exist", HelpBase)>]
let argNamesCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0061" "ArgNames" (fun () ->
        argNamesMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0063 / FR0064 ExceptionRules ----

let private exceptionRulesMessages (fileName: string) (parseTree: ParsedInput) (source: ISourceText) : Message list =
    let finallyEnabled = Configuration.isRuleEnabled fileName "FR0063" "RaiseInFinally"

    let reservedEnabled =
        Configuration.isRuleEnabled fileName "FR0064" "ReservedException"

    if not (finallyEnabled || reservedEnabled) then
        []
    else
        let finallies, reserved = ExceptionRules.find parseTree source

        let finallyMessages =
            if finallyEnabled then
                finallies
                |> List.map (fun s ->
                    hint
                        "FR0063"
                        "Raising inside finally replaces any exception already in flight — the original failure vanishes."
                        s.Range
                        [])
            else
                []

        let reservedMessages =
            if reservedEnabled then
                reserved
                |> List.map (fun s ->
                    hint
                        "FR0064"
                        $"%s{s.TypeName} is reserved for the runtime; raising it manually misleads catchers and debuggers — InvalidOperationException or an Argument exception says what actually happened."
                        s.Range
                        [])
            else
                []

        finallyMessages @ reservedMessages

[<EditorAnalyzer("ExceptionRules", "Raise-in-finally and reserved exceptions", HelpBase)>]
let exceptionRulesEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return exceptionRulesMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText }

[<CliAnalyzer("ExceptionRules", "Raise-in-finally and reserved exceptions", HelpBase)>]
let exceptionRulesCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return exceptionRulesMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText }

// ---- FR0065 / FR0066 SecurityRules ----

let private securityRulesMessages
    (fileName: string)
    (parseTree: ParsedInput)
    (source: ISourceText)
    (offerAlternatives: bool)
    : Message list =
    let cryptoEnabled = Configuration.isRuleEnabled fileName "FR0065" "WeakCrypto"
    let sqlEnabled = Configuration.isRuleEnabled fileName "FR0066" "SqlStrings"
    let processEnabled = Configuration.isRuleEnabled fileName "FR0126" "ProcessSinks"

    if not (cryptoEnabled || sqlEnabled || processEnabled) then
        []
    else
        let crypto, sql, processSinks = SecurityRules.find parseTree source

        let processMessages =
            if processEnabled then
                processSinks
                |> List.map (fun s ->
                    hint
                        "FR0126"
                        $"A dynamically built string reaches {s.Sink} — the command/argument-injection sink, and doubly so when the string carries LLM or agent output; pass a fixed executable with an argument LIST (ProcessStartInfo.ArgumentList) instead."
                        s.Range
                        [])
            else
                []

        let cryptoMessages =
            if cryptoEnabled then
                crypto
                |> List.collect (fun s ->
                    // SHA1 only: MD5-as-checksum is a legitimate non-security
                    // use, and swapping any persisted hash algorithm is an
                    // API-shaped change — so this is an EDITOR action, never
                    // CLI-applied
                    match s.Kind, s.AlgoRange with
                    | SecurityRules.WeakKind.Hash "SHA1", Some algo when offerAlternatives ->
                        [ hint
                              "FR0065"
                              "Alternative: switch to SHA256 (mind persisted hashes and interop — the output size changes)."
                              s.Range
                              [ fix algo "SHA1" "SHA256" ]
                          hint
                              "FR0065"
                              "Alternative: switch to SHA512 (mind persisted hashes and interop — the output size changes)."
                              s.Range
                              [ fix algo "SHA1" "SHA512" ] ]
                    // flags-OR is idempotent, so the swap is safe even in a
                    // `Tls ||| Tls12` chain — but dropping a legacy protocol
                    // can still surprise an ancient endpoint, so it stays an
                    // editor action
                    | SecurityRules.WeakKind.Protocol proto, Some ident when offerAlternatives ->
                        [ hint
                              "FR0065"
                              $"Alternative: replace {proto} with Tls12 (mind endpoints that only speak the legacy protocol)."
                              s.Range
                              [ fix ident proto "Tls12" ] ]
                    | _ -> [])
                |> List.append (
                    crypto
                    |> List.map (fun s ->
                    let message =
                        match s.Kind with
                        | SecurityRules.WeakKind.Hash name ->
                            $"%s{name} is collision-broken for security purposes; use SHA-256 or stronger (for non-security checksums, note the intent)."
                        | SecurityRules.WeakKind.Cipher name ->
                            $"%s{name}'s key size is within practical attack range; use AES."
                        | SecurityRules.WeakKind.CertificateBypass ->
                            "Overriding certificate validation silently accepts any man-in-the-middle; scope trust to the specific expected certificate instead."
                        | SecurityRules.WeakKind.Protocol name ->
                            $"%s{name} is broken or deprecated on the wire; prefer setting nothing (the OS negotiates the strongest protocol) or Tls12+."

                    hint "FR0065" message s.Range [])
                )
            else
                []

        let sqlMessages =
            if sqlEnabled then
                sql
                |> List.map (fun s ->
                    hint
                        "FR0066"
                        (sprintf
                            "SQL assembled from strings flows user data into the query language (%s); use parameters (@name + Parameters.Add) instead."
                            s.Sink)
                        s.Range
                        [])
            else
                []

        cryptoMessages @ sqlMessages @ processMessages

[<EditorAnalyzer("SecurityRules", "Weak crypto and string-built SQL", HelpBase)>]
let securityRulesEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return securityRulesMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText true }

[<CliAnalyzer("SecurityRules", "Weak crypto and string-built SQL", HelpBase)>]
let securityRulesCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return securityRulesMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText false }

// ---- FR0125 UnicodeHygiene ----

let private unicodeMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    UnicodeHygiene.find parseTree source
    |> List.map (fun s ->
        let fixes =
            match s.Fix with
            | Some(r, original, replacement) -> [ fix r original replacement ]
            | None -> []

        hint
            "FR0125"
            $"Invisible character {s.CodePoint} ({s.FamilyName}) — it cannot be seen in review, which is exactly how Trojan Source and prompt-smuggling work; spell it as an escape or remove it."
            s.Range
            fixes)

[<EditorAnalyzer("UnicodeHygiene", "Invisible and bidirectional Unicode in source", HelpBase)>]
let unicodeEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0125" "UnicodeHygiene" (fun () ->
        unicodeMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("UnicodeHygiene", "Invisible and bidirectional Unicode in source", HelpBase)>]
let unicodeCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0125" "UnicodeHygiene" (fun () ->
        unicodeMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0127 SecretLiterals ----

let private secretMessages (parseTree: ParsedInput) : Message list =
    SecretLiterals.find parseTree
    |> List.map (fun s ->
        hint
            "FR0127"
            $"This literal matches {s.Provider}'s documented credential format — a leaked key until proven otherwise; rotate it and move it to configuration or a secret store."
            s.Range
            [])

[<EditorAnalyzer("SecretLiterals", "Provider-format API keys in string literals", HelpBase)>]
let secretsEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0127" "SecretLiterals" (fun () -> secretMessages ctx.ParseFileResults.ParseTree)

[<CliAnalyzer("SecretLiterals", "Provider-format API keys in string literals", HelpBase)>]
let secretsCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0127" "SecretLiterals" (fun () -> secretMessages ctx.ParseFileResults.ParseTree)

// ---- FR0128 ObsoleteCrypto ----

let private obsoleteCryptoMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    ObsoleteCrypto.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0128"
            $"'{s.ObsoleteName}' is the obsolete constructor spelling (SYSLIB0021); the static factory picks the platform implementation of the SAME algorithm."
            s.Range
            [ fix s.Range (Text.textOfRange source s.Range) s.Replacement ])

[<EditorAnalyzer("ObsoleteCrypto", "Obsolete crypto constructors become static factories", HelpBase)>]
let obsoleteCryptoEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0128" "ObsoleteCrypto" (fun () ->
        obsoleteCryptoMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("ObsoleteCrypto", "Obsolete crypto constructors become static factories", HelpBase)>]
let obsoleteCryptoCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0128" "ObsoleteCrypto" (fun () ->
        obsoleteCryptoMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0129 MatchGuards ----

let private matchGuardsMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    MatchGuards.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0129"
            $"The guard only equality-tests '{s.BinderName}' against {s.LiteralText} — that IS the literal pattern."
            s.Range
            [ fix s.Range (Text.textOfRange source s.Range) s.LiteralText ])

[<EditorAnalyzer("MatchGuards", "A guard that only equality-tests the binder is the literal pattern", HelpBase)>]
let matchGuardsEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0129" "MatchGuards" (fun () ->
        matchGuardsMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("MatchGuards", "A guard that only equality-tests the binder is the literal pattern", HelpBase)>]
let matchGuardsCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0129" "MatchGuards" (fun () ->
        matchGuardsMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0130 LiteralConst ----

let private literalConstMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    LiteralConst.find (Visibility.apiChangesAllowed ()) parseTree source
    |> List.map (fun s ->
        let insertRange, text = s.Fix

        hint
            "FR0130"
            $"'{s.Name}' is a compile-time constant; [<Literal>] lets it serve in patterns and attribute arguments and const-folds at use sites."
            s.Range
            [ fix insertRange "" text ])

[<EditorAnalyzer("LiteralConst", "Module-level constants gain [<Literal>]", HelpBase)>]
let literalConstEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0130" "LiteralConst" (fun () ->
        literalConstMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("LiteralConst", "Module-level constants gain [<Literal>]", HelpBase)>]
let literalConstCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0130" "LiteralConst" (fun () ->
        literalConstMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0131 RecTailCall ----

let private recTailCallMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    RecTailCall.find parseTree source checkResults
    |> List.map (fun s ->
        let insertRange, text = s.Fix

        hint
            "FR0131"
            $"every recursive call in '{s.Name}' sits in tail position; [<TailCall>] makes the compiler warn (FS3569) if a later edit changes that."
            s.Range
            [ fix insertRange "" text ])

[<EditorAnalyzer("RecTailCall", "Provably tail-recursive functions gain [<TailCall>]", HelpBase)>]
let recTailCallEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0131" "RecTailCall" (fun () ->
        whenChecked ctx (recTailCallMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("RecTailCall", "Provably tail-recursive functions gain [<TailCall>]", HelpBase)>]
let recTailCallCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0131" "RecTailCall" (fun () ->
        recTailCallMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0132 CommentDoc ----

let private commentDocMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    CommentDoc.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0132"
            $"this public {s.What} has no XML doc, but its trailing comment says exactly what one would; promoted to /// it reaches tooltips and generated docs."
            s.Range
            (s.Edits |> List.map (fun (r, original, replacement) -> fix r original replacement)))

[<EditorAnalyzer("CommentDoc", "Trailing comments promoted to XML doc position", HelpBase)>]
let commentDocEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0132" "CommentDoc" (fun () ->
        commentDocMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("CommentDoc", "Trailing comments promoted to XML doc position", HelpBase)>]
let commentDocCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0132" "CommentDoc" (fun () ->
        commentDocMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0133 NameQuoting ----

let private nameQuotingMessages
    (fileName: string)
    (parseTree: ParsedInput)
    (source: ISourceText)
    checkResults
    projectCheck
    : Message list =
    // test-attributed names rewrite by default; local and file-private
    // names are the config opt-in:  { "FR0133": { "locals": 1 } }
    let includeLocals =
        Configuration.parameterInt fileName "FR0133" "NameQuoting" "locals" 0 = 1

    NameQuoting.find includeLocals parseTree source checkResults projectCheck
    |> List.map (fun s ->
        hint
            "FR0133"
            $"'{s.Name}' is {s.Name.Length} characters of camel case; the double-backtick name ``{s.Quoted}`` reads as the sentence it is — renamed at its definition and every use."
            s.Range
            (s.Edits |> List.map (fun (r, original, replacement) -> fix r original replacement)))

[<EditorAnalyzer("NameQuoting", "Five-word names become double-backtick names", HelpBase)>]
let nameQuotingEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0133" "NameQuoting" (fun () ->
        whenChecked ctx (fun check ->
            nameQuotingMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText check None))

[<CliAnalyzer("NameQuoting", "Five-word names become double-backtick names", HelpBase)>]
let nameQuotingCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0133" "NameQuoting" (fun () ->
        nameQuotingMessages
            ctx.FileName
            ctx.ParseFileResults.ParseTree
            ctx.SourceText
            ctx.CheckFileResults
            (Some ctx.CheckProjectResults))

// ---- FR0134 DateTimeOffsetMigration ----

let private dateTimeOffsetMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    DateTimeOffsetMigration.find (Visibility.apiChangesAllowed ()) parseTree source
    |> List.choose (fun s ->
        if s.IsFilePrivate then
            DateTimeOffsetMigration.migrate parseTree source checkResults s
            |> Option.map (fun edits ->
                hint
                    "FR0134"
                    $"Field '{s.FieldName}: DateTime' of the file-private type '{s.TypeName}' drops the clock it was read from; every write and read fits DateTimeOffset, which keeps the instant AND its offset — migrated in one edit set."
                    s.Range
                    (edits |> List.map (fun (r, original, replacement) -> fix r original replacement)))
        else
            None)

[<EditorAnalyzer("DateTimeOffsetMigration", "DateTime record fields migrate to DateTimeOffset", HelpBase)>]
let dateTimeOffsetEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0134" "DateTimeOffsetMigration" (fun () ->
        whenChecked ctx (dateTimeOffsetMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("DateTimeOffsetMigration", "DateTime record fields migrate to DateTimeOffset", HelpBase)>]
let dateTimeOffsetCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0134" "DateTimeOffsetMigration" (fun () ->
        dateTimeOffsetMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0135 LiterateComment ----

let private literateCommentMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    LiterateComment.find parseTree source
    |> List.map (fun s ->
        let r, text = s.Fix

        hint
            "FR0135"
            $"this block comment carries {s.Evidence} — markdown FSharp.Formatting silently drops from a plain comment; one more star makes it the literate cell it reads as."
            s.Range
            [ fix r "" text ])

[<EditorAnalyzer("LiterateComment", "Markdown-bearing script comments become literate cells", HelpBase)>]
let literateCommentEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0135" "LiterateComment" (fun () ->
        literateCommentMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("LiterateComment", "Markdown-bearing script comments become literate cells", HelpBase)>]
let literateCommentCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0135" "LiterateComment" (fun () ->
        literateCommentMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0136 EmptyGuid ----

let private emptyGuidMessages
    (parseTree: ParsedInput)
    (source: ISourceText)
    (offerAlternatives: bool)
    checkResults
    : Message list =
    EmptyGuid.find parseTree source checkResults
    |> List.collect (fun s ->
        let original = Text.textOfRange source s.Range

        [ hint
              "FR0136"
              $"the zero-argument Guid constructor is 00000000-…: if the empty value is intended, {s.EmptyText} says so; if a FRESH guid was meant, this is the classic .NET slip."
              s.Range
              [ fix s.Range original s.EmptyText ]
          // the behavior-CHANGING repair — the likely intent, but only a
          // human knows; never CLI-applied
          if offerAlternatives then
              hint
                  "FR0136"
                  $"Alternative: {s.NewGuidText} — if a fresh guid was the intent, this is the actual bug fix."
                  s.Range
                  [ fix s.Range original s.NewGuidText ] ])

[<EditorAnalyzer("EmptyGuid", "Zero-argument Guid constructors state Empty or become NewGuid", HelpBase)>]
let emptyGuidEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0136" "EmptyGuid" (fun () ->
        whenChecked ctx (emptyGuidMessages ctx.ParseFileResults.ParseTree ctx.SourceText true))

[<CliAnalyzer("EmptyGuid", "Zero-argument Guid constructors state Empty or become NewGuid", HelpBase)>]
let emptyGuidCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0136" "EmptyGuid" (fun () ->
        emptyGuidMessages ctx.ParseFileResults.ParseTree ctx.SourceText false ctx.CheckFileResults)

// ---- FR0137 MapFusion ----

let private mapFusionMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    MapFusion.find parseTree source
    |> List.map (fun s ->
        let message =
            if s.Module = "Seq" then
                "These two Seq.map stages can fuse into one, removing a lazy wrapper."
            else
                $"These two {s.Module}.map passes can fuse into one, avoiding an intermediate {s.Module.ToLowerInvariant()}."

        hint "FR0137" message s.Range [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("MapFusion", "Fuse consecutive map passes with function composition", HelpBase)>]
let mapFusionEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0137" "MapFusion" (fun () ->
        mapFusionMessages ctx.ParseFileResults.ParseTree ctx.SourceText
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("MapFusion", "Fuse consecutive map passes with function composition", HelpBase)>]
let mapFusionCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0137" "MapFusion" (fun () ->
        mapFusionMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0138 StringEmptiness ----

let private stringEmptinessMessages
    (offerAlternatives: bool)
    (parseTree: ParsedInput)
    (source: ISourceText)
    : Message list =
    StringEmptiness.find parseTree source
    |> List.collect (fun s ->
        let predicate =
            if s.WhiteSpace then
                "String.IsNullOrWhiteSpace"
            else
                "String.IsNullOrEmpty"

        if s.Guarded then
            [ hint
                  "FR0138"
                  $"this hand-rolled emptiness test IS {predicate} — the null guard short-circuits exactly as the predicate answers, and the Trim spellings stop allocating a trimmed copy."
                  s.Range
                  [ fix s.Range s.OriginalText s.ReplacementText ] ]
        else
            // null behavior changes: the original throws, the predicate
            // answers true. Almost always the intent — but a human signs
            [ hint
                  "FR0138"
                  $"trimming a copy just to test it: {predicate} tests the same whitespace set without allocating — but it answers true for null where this throws, so apply deliberately."
                  s.Range
                  (if offerAlternatives then
                       [ fix s.Range s.OriginalText s.ReplacementText ]
                   else
                       []) ])

[<EditorAnalyzer("StringEmptiness", "Hand-rolled emptiness tests become the BCL predicates", HelpBase)>]
let stringEmptinessEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0138" "StringEmptiness" (fun () ->
        stringEmptinessMessages true ctx.ParseFileResults.ParseTree ctx.SourceText
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("StringEmptiness", "Hand-rolled emptiness tests become the BCL predicates", HelpBase)>]
let stringEmptinessCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0138" "StringEmptiness" (fun () ->
        stringEmptinessMessages false ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0062 / FR0067 / FR0068 MiscRules ----

let private miscRulesMessages
    (fileName: string)
    (parseTree: ParsedInput)
    (source: ISourceText)
    (offerAlternatives: bool)
    : Message list =
    let mutableEnabled =
        Configuration.isRuleEnabled fileName "FR0062" "VisibleMutableState"

    let parseEnabled = Configuration.isRuleEnabled fileName "FR0067" "CultureParse"
    let enumEnabled = Configuration.isRuleEnabled fileName "FR0068" "DuplicateEnumValue"

    if not (mutableEnabled || parseEnabled || enumEnabled) then
        []
    else
        let mutables, parses, enums = MiscRules.find parseTree source

        let mutableMessages =
            if mutableEnabled then
                mutables
                |> List.map (fun s ->
                    hint
                        "FR0062"
                        (sprintf
                            "'%s' is visible mutable module state — a global variable any consumer can write, with no thread safety; make it private or pass the state explicitly."
                            s.Name)
                        s.Range
                        [])
            else
                []

        let parseMessages =
            if parseEnabled then
                // wire/config data wants InvariantCulture — the clear
                // default; spelling out CurrentCulture is the alternative
                // when today's implicit behavior WAS the intent. The CLI
                // auto-applies invariant only on the config opt-in:
                //     { "FR0067": { "invariant": 1 } }
                let autoInvariant =
                    Configuration.parameterInt fileName "FR0067" "MiscRules" "invariant" 0 = 1

                parses
                |> List.collect (fun s ->
                    let note =
                        hint
                            "FR0067"
                            (sprintf
                                "%s without a culture reads differently under different server cultures ('1,5' vs '1.5', day/month order); pass CultureInfo.InvariantCulture or the intended culture explicitly."
                                s.CallName)
                            s.Range
                            (match s.CultureFix with
                             | Some mk when autoInvariant ->
                                 let r, original, replacement =
                                     mk "InvariantCulture"

                                 [ fix r original replacement ]
                             | _ -> [])

                    match s.CultureFix with
                    | Some mk when offerAlternatives && not autoInvariant ->
                        let ri, oi, pi = mk "InvariantCulture"
                        let rc, oc, pc = mk "CurrentCulture"

                        [ note
                          hint "FR0067" "Fix: parse with InvariantCulture (wire and config data)." s.Range [ fix ri oi pi ]
                          hint
                              "FR0067"
                              "Alternative: spell out CurrentCulture — today's implicit behavior, made deliberate."
                              s.Range
                              [ fix rc oc pc ] ]
                    | _ -> [ note ])
            else
                []

        let enumMessages =
            if enumEnabled then
                enums
                |> List.map (fun s ->
                    hint
                        "FR0068"
                        (sprintf
                            "Enum case '%s' has the same value as '%s'; comparisons and ToString silently conflate them — usually a copy-paste slip."
                            s.CaseName
                            s.OriginalName)
                        s.Range
                        [])
            else
                []

        mutableMessages @ parseMessages @ enumMessages

[<EditorAnalyzer("MiscRules", "Visible mutable state, culture parsing, duplicate enum values", HelpBase)>]
let miscRulesEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return miscRulesMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText true }

[<CliAnalyzer("MiscRules", "Visible mutable state, culture parsing, duplicate enum values", HelpBase)>]
let miscRulesCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return miscRulesMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText false }

// ---- FR0069 / FR0070 StructHints ----

let private structHintsMessages
    (fileName: string)
    (parseTree: ParsedInput)
    (source: ISourceText)
    (checkOpt: FSharpCheckFileResults option)
    (projectCheck: FSharpCheckProjectResults option)
    : Message list =
    let voptionEnabled = Configuration.isRuleEnabled fileName "FR0069" "VOptionField"
    let structEnabled = Configuration.isRuleEnabled fileName "FR0070" "SmallStructType"

    let structTupleEnabled =
        Configuration.isRuleEnabled fileName "FR0093" "StructTupleField"

    if not (voptionEnabled || structEnabled || structTupleEnabled) then
        []
    else
        let voptions, structs, structTuples =
            StructHints.find (Visibility.apiChangesAllowed ()) parseTree source

        let voptionMessages =
            if voptionEnabled then
                voptions
                |> List.map (fun s ->
                    // a strictly file-private field migrates as ONE edit
                    // set: field type plus every use, all in this file by
                    // construction. Any use outside the provably-
                    // rewritable shapes keeps it a note
                    let migration =
                        match checkOpt with
                        | Some check when s.IsFilePrivate ->
                            VOptionMigration.migrate parseTree source check s.FieldIdRange s.FieldName s.OptionNameRange
                        | Some check when Visibility.apiChangesAllowed () && s.IsConfined ->
                            // a strictly INTERNAL field under --api-changes:
                            // every use in the project classified against its
                            // own file, one edit set spanning files. Public
                            // fields never take this path (consumers can sit
                            // in a sibling project no scan sees), and neither
                            // does an assembly that opens its internals to
                            // friends
                            projectCheck
                            |> Option.filter (ProjectSources.hasInternalsVisibleTo >> not)
                            |> Option.bind (fun pc ->
                                VOptionMigration.migrateProject
                                    parseTree
                                    source
                                    check
                                    pc
                                    s.FieldIdRange
                                    s.FieldName
                                    s.OptionNameRange)
                        | _ -> None

                    let containment = if s.IsFilePrivate then "file-private" else "internal"

                    match migration with
                    | Some edits ->
                        hint
                            "FR0069"
                            $"Field '%s{s.FieldName}: %s{s.ElementText} option' of the %s{containment} type '%s{s.TypeName}' boxes the %s{s.ElementText} on every Some; the fix migrates the field and its %d{edits.Length - 1} use(s) to '%s{s.ElementText} voption'."
                            s.Range
                            (edits |> List.map (fun (r, original, replacement) -> fix r original replacement))
                    | None ->
                        hint
                            "FR0069"
                            $"Field '%s{s.FieldName}: %s{s.ElementText} option' of the contained type '%s{s.TypeName}' boxes the %s{s.ElementText} on every Some; '%s{s.ElementText} voption' keeps it flat — and private/internal visibility keeps the migration contained (public types risk serialization changes and unbounded call-site churn)."
                            s.Range
                            [])
            else
                []

        let structMessages =
            if structEnabled then
                structs
                |> List.map (fun s ->
                    let fixes =
                        match s.Fix with
                        | Some(r, text) -> [ fix r "" text ]
                        | None -> []

                    hint
                        "FR0070"
                        $"Contained record '%s{s.TypeName}' has only %d{s.FieldCount} small struct field(s); [<Struct>] removes a heap allocation per instance (mind copy semantics: struct records copy on assignment)."
                        s.Range
                        fixes)
            else
                []

        let structTupleMessages =
            if structTupleEnabled then
                structTuples
                |> List.map (fun s ->
                    // a strictly file-private field migrates as ONE edit set:
                    // field type plus every construction/destructuring, all
                    // in this file by construction
                    let migration =
                        match checkOpt with
                        | Some check when s.IsFilePrivate ->
                            StructTupleMigration.migrate parseTree source check s.FieldIdRange s.FieldName s.Range
                        | Some check when Visibility.apiChangesAllowed () && s.IsConfined ->
                            projectCheck
                            |> Option.filter (ProjectSources.hasInternalsVisibleTo >> not)
                            |> Option.bind (fun pc ->
                                StructTupleMigration.migrateProject
                                    parseTree
                                    source
                                    check
                                    pc
                                    s.FieldIdRange
                                    s.FieldName
                                    s.Range)
                        | _ -> None

                    let containment = if s.IsFilePrivate then "file-private" else "internal"

                    match migration with
                    | Some edits ->
                        hint
                            "FR0093"
                            $"Field '%s{s.FieldName}: %s{s.TupleText}' of the %s{containment} type '%s{s.TypeName}' is a reference tuple: one heap object per value; the fix migrates the field and its %d{edits.Length - 1} use(s) to 'struct (%s{s.TupleText})'."
                            s.Range
                            (edits |> List.map (fun (r, original, replacement) -> fix r original replacement))
                    | None ->
                        hint
                            "FR0093"
                            $"Field '%s{s.FieldName}: %s{s.TupleText}' of the contained type '%s{s.TypeName}' is a reference tuple: one heap object per value. 'struct (%s{s.TupleText})' stores it inline — but every construction and destructuring of the field needs the struct keyword too, so this is advice, not a mechanical fix."
                            s.Range
                            [])
            else
                []

        voptionMessages @ structMessages @ structTupleMessages

[<EditorAnalyzer("StructHints", "voption fields and [<Struct>] candidates in contained types", HelpBase)>]
let structHintsEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async {
        // no ProjectSources host in editors: the cross-file path degrades
        // to the note by itself
        return
            structHintsMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults None
    }

[<CliAnalyzer("StructHints", "voption fields and [<Struct>] candidates in contained types", HelpBase)>]
let structHintsCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async {
        // parse-only runs carry degraded check results; the migration
        // itself refuses to run on error files, so passing them is safe
        return
            structHintsMessages
                ctx.FileName
                ctx.ParseFileResults.ParseTree
                ctx.SourceText
                (Some ctx.CheckFileResults)
                (Some ctx.CheckProjectResults)
    }

// ---- FR0071 LoopInvariant ----

let private loopInvariantMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    LoopInvariant.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0071"
            $"'let %s{s.Name} = ...' does not depend on the loop, but every iteration re-evaluates it; the rewrite hoists it above the loop (the value is pure, so evaluating it once is the only observable change — a saving)."
            s.Range
            [ for range, original, replacement in s.Edits -> fix range original replacement ])

[<EditorAnalyzer("LoopInvariant", "Hoist pure loop-invariant bindings out of loops", HelpBase)>]
let loopInvariantEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0071" "LoopInvariant" (fun () ->
        whenChecked ctx (loopInvariantMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("LoopInvariant", "Hoist pure loop-invariant bindings out of loops", HelpBase)>]
let loopInvariantCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0071" "LoopInvariant" (fun () ->
        loopInvariantMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0072 ExpandWildcard ----

let private expandWildcardMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    ExpandWildcard.find parseTree source checkResults
    |> List.map (fun s ->
        let hidden = String.concat ", " s.HiddenCases

        hint
            "FR0072"
            $"This wildcard stands in for exactly %s{hidden}; matching explicitly keeps the match closed, so a future union case raises an incomplete-match warning instead of silently taking this branch."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("ExpandWildcard", "Expand a wildcard hiding one or two DU cases", HelpBase)>]
let expandWildcardEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0072" "ExpandWildcard" (fun () ->
        whenChecked ctx (expandWildcardMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("ExpandWildcard", "Expand a wildcard hiding one or two DU cases", HelpBase)>]
let expandWildcardCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0072" "ExpandWildcard" (fun () ->
        expandWildcardMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0073 MatchBang ----

let private matchBangMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    MatchBangRule.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0073"
            $"'{s.Name}' exists only to be matched; 'match!' binds and matches in one step (F# 4.5+)."
            s.Range
            [ for range, original, replacement in s.Edits -> fix range original replacement ])

[<EditorAnalyzer("MatchBang", "Collapse let!-then-match into match!", HelpBase)>]
let matchBangEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0073" "MatchBang" (fun () ->
        matchBangMessages ctx.ParseFileResults.ParseTree ctx.SourceText
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("MatchBang", "Collapse let!-then-match into match!", HelpBase)>]
let matchBangCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0073" "MatchBang" (fun () ->
        matchBangMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0078 WhileBang ----

/// Does the project's --langversion allow at least this F# major version?
/// An absent flag means the SDK default, which is the latest.
let private langVersionAtLeast (major: float) (options: AnalyzerProjectOptions) =
    let explicitVersion =
        options.OtherOptions
        |> List.tryPick (fun (arg: string) ->
            if arg.StartsWith "--langversion:" then
                Some(arg.Substring "--langversion:".Length)
            else
                None)

    match explicitVersion with
    | None
    | Some("latest" | "preview" | "latestmajor") -> true
    | Some v ->
        match
            System.Double.TryParse(
                v,
                System.Globalization.NumberStyles.Float,
                System.Globalization.CultureInfo.InvariantCulture
            )
        with
        | true, n -> n >= major
        | false, _ -> false

let private whileBangMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    MatchBangRule.findWhileBang parseTree source
    |> List.map (fun s ->
        hint
            "FR0078"
            $"This mutable-'%s{s.Name}' loop is the F# 8 'while!' idiom spelled out; 'while!' re-evaluates the computation each iteration, replacing all three bindings."
            s.Range
            [ for range, original, replacement in s.Edits -> fix range original replacement ])

[<EditorAnalyzer("WhileBang", "Collapse the mutable-condition loop idiom into while! (F# 8)", HelpBase)>]
let whileBangEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0078" "WhileBang" (fun () ->
        if langVersionAtLeast 8.0 ctx.ProjectOptions then
            whileBangMessages ctx.ParseFileResults.ParseTree ctx.SourceText
            |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText
        else
            [])

[<CliAnalyzer("WhileBang", "Collapse the mutable-condition loop idiom into while! (F# 8)", HelpBase)>]
let whileBangCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0078" "WhileBang" (fun () ->
        if langVersionAtLeast 8.0 ctx.ProjectOptions then
            whileBangMessages ctx.ParseFileResults.ParseTree ctx.SourceText
        else
            [])

// ---- FR0074 NestedRecordUpdate ----

let private nestedRecordUpdateMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    NestedRecordUpdate.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0074"
            $"F# 8 updates nested fields directly: this copy-and-update chain flattens to '%s{s.Path}' path syntax."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("NestedRecordUpdate", "Flatten nested record copy-and-update (F# 8)", HelpBase)>]
let nestedRecordUpdateEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0074" "NestedRecordUpdate" (fun () ->
        if langVersionAtLeast 8.0 ctx.ProjectOptions then
            whenChecked ctx (nestedRecordUpdateMessages ctx.ParseFileResults.ParseTree ctx.SourceText)
            |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText
        else
            [])

[<CliAnalyzer("NestedRecordUpdate", "Flatten nested record copy-and-update (F# 8)", HelpBase)>]
let nestedRecordUpdateCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0074" "NestedRecordUpdate" (fun () ->
        if langVersionAtLeast 8.0 ctx.ProjectOptions then
            nestedRecordUpdateMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults
        else
            [])

// ---- FR0075 UseBinding ----

let private useBindingMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    UseBinding.find parseTree source checkResults
    |> List.map (fun s ->
        match s.Fix with
        | Some(original, replacement) ->
            hint
                "FR0075"
                $"'%s{s.Name}' is a locally constructed disposable that nothing disposes; 'use' disposes it at scope exit, and every mention stays inside the scope."
                s.Range
                [ fix s.Range original replacement ]
        | None ->
            hint
                "FR0075"
                $"'%s{s.Name}' is a locally constructed disposable that nothing disposes; it also escapes this scope (passed, stored, or returned), so decide the owner — 'use' here, or disposal at the destination."
                s.Range
                [])

[<EditorAnalyzer("UseBinding", "Locally constructed disposables become use-bindings", HelpBase)>]
let useBindingEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0075" "UseBinding" (fun () ->
        whenChecked ctx (useBindingMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("UseBinding", "Locally constructed disposables become use-bindings", HelpBase)>]
let useBindingCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0075" "UseBinding" (fun () ->
        useBindingMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0076 MapIgnore ----

let private mapIgnoreMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    MapIgnore.find parseTree source checkResults
    |> List.map (fun s ->
        match s.ReplacementText with
        | Some replacement ->
            hint
                "FR0076"
                $"%s{s.ModuleName}.map allocates a result list just to discard it; iter runs the same calls in the same order without it."
                s.Range
                [ fix s.Range s.OriginalText replacement ]
        | None ->
            hint
                "FR0076"
                "Seq.map is lazy: piping it to ignore evaluates nothing — the mapping never runs. Seq.iter would run the effects; if none are wanted, delete the line."
                s.Range
                [])

[<EditorAnalyzer("MapIgnore", "map-then-ignore pipelines", HelpBase)>]
let mapIgnoreEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0076" "MapIgnore" (fun () ->
        whenChecked ctx (mapIgnoreMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("MapIgnore", "map-then-ignore pipelines", HelpBase)>]
let mapIgnoreCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0076" "MapIgnore" (fun () ->
        mapIgnoreMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0092 FailwithContext ----

let private failwithContextMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    FailwithContext.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0092"
            $"This failure message is a constant: every occurrence in the log reads the same. Interpolating %s{s.FunctionName}'s arguments says which call produced it — check the values are safe to log first."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("FailwithContext", "Static failwith messages that could carry their arguments", HelpBase)>]
let failwithContextEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0092" "FailwithContext" (fun () ->
        whenChecked ctx (failwithContextMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("FailwithContext", "Static failwith messages that could carry their arguments", HelpBase)>]
let failwithContextCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0092" "FailwithContext" (fun () ->
        failwithContextMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0079 SingleAwaitable ----

let private singleAwaitableMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    SingleAwaitable.find parseTree source checkResults
    |> List.map (fun s ->
        let advice =
            if s.CallName = "Async.Parallel" then
                "nothing runs in parallel — run the one computation directly (the result becomes 'T instead of 'T[])"
            else
                "await the one task directly (the task keeps its result where WhenAll returns plain Task)"

        hint
            "FR0079"
            $"%s{s.CallName} over a single-element literal adds indirection for nothing; %s{advice}."
            s.Range
            [])

[<EditorAnalyzer("SingleAwaitable", "WhenAll/WaitAll/Parallel over a single awaitable", HelpBase)>]
let singleAwaitableEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0079" "SingleAwaitable" (fun () ->
        whenChecked ctx (singleAwaitableMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("SingleAwaitable", "WhenAll/WaitAll/Parallel over a single awaitable", HelpBase)>]
let singleAwaitableCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0079" "SingleAwaitable" (fun () ->
        singleAwaitableMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0077 ImplementMissing ----

let private implementMissingMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    // deliberately NO hasErrors gate: this rule exists to fix the
    // missing-members compile error
    ImplementMissing.find parseTree source checkResults
    |> List.map (fun s ->
        let missing = String.concat ", " s.MissingNames

        hint
            "FR0077"
            $"This object expression is missing %d{s.MissingNames.Length} member(s) of %s{s.InterfaceName} (%s{missing}); the fix stubs them with NotImplementedException so the code compiles and the TODOs are explicit."
            s.Range
            [ fix s.Range "" s.InsertText ])

[<EditorAnalyzer("ImplementMissing", "Stub missing interface members with NotImplementedException", HelpBase)>]
let implementMissingEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0077" "ImplementMissing" (fun () ->
        whenChecked ctx (implementMissingMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("ImplementMissing", "Stub missing interface members with NotImplementedException", HelpBase)>]
let implementMissingCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0077" "ImplementMissing" (fun () ->
        implementMissingMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0080 TabIndentation ----

let private tabIndentationMessages (fileName: string) (source: ISourceText) : Message list =
    // works from source text alone: tab-indented files do not parse
    // (FS1161), and repairing that is the point
    TabIndentation.find fileName source
    |> List.map (fun s ->
        hint
            "FR0080"
            $"TABs are not allowed as F# indentation (FS1161) — pasted code often brings them along; the fix expands each leading TAB to four spaces on all %d{s.Edits.Length} affected line(s)."
            s.Range
            [ for range, original, replacement in s.Edits -> fix range original replacement ])

[<EditorAnalyzer("TabIndentation", "Expand pasted TAB indentation to spaces", HelpBase)>]
let tabIndentationEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0080" "TabIndentation" (fun () -> tabIndentationMessages ctx.FileName ctx.SourceText)

[<CliAnalyzer("TabIndentation", "Expand pasted TAB indentation to spaces", HelpBase)>]
let tabIndentationCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0080" "TabIndentation" (fun () -> tabIndentationMessages ctx.FileName ctx.SourceText)

// ---- FR0081 PathSeparator ----

let private pathSeparatorMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    PathSeparator.find parseTree source
    |> List.map (fun s ->
        let sep = if s.Separator = "/" then "'/'" else "'\\'"

        hint
            "FR0081"
            $"This concatenation joins path fragments with a hard-coded %s{sep}; Path.Combine handles separators and platform differences (advice: it treats a ROOTED second argument as absolute, and a URL should stay string-joined or use Uri)."
            s.Range
            [])

[<EditorAnalyzer("PathSeparator", "Hand-built path concatenation", HelpBase)>]
let pathSeparatorEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0081" "PathSeparator" (fun () ->
        pathSeparatorMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("PathSeparator", "Hand-built path concatenation", HelpBase)>]
let pathSeparatorCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0081" "PathSeparator" (fun () ->
        pathSeparatorMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0082 / FR0083 / FR0084 / FR0086 RedundantSyntax ----

let private redundantSyntaxMessages (fileName: string) (parseTree: ParsedInput) (source: ISourceText) : Message list =
    let codeOf kind =
        match kind with
        | RedundantSyntax.Kind.AttributeSuffix -> "FR0082", "AttributeSuffix"
        | RedundantSyntax.Kind.AttributeParens -> "FR0083", "AttributeParens"
        | RedundantSyntax.Kind.Backticks -> "FR0084", "RedundantBackticks"
        | RedundantSyntax.Kind.HoleFreeInterpolation -> "FR0086", "HoleFreeInterpolation"

    let messageOf kind =
        match kind with
        | RedundantSyntax.Kind.AttributeSuffix ->
            "The Attribute suffix is redundant; the compiler resolves the short form."
        | RedundantSyntax.Kind.AttributeParens -> "An empty argument list on an attribute says nothing."
        | RedundantSyntax.Kind.Backticks -> "These backticks quote a plain identifier; the quoting does nothing."
        | RedundantSyntax.Kind.HoleFreeInterpolation ->
            "This interpolated string has no holes; a plain string literal says the same with less."

    RedundantSyntax.find parseTree source
    |> List.choose (fun s ->
        let code, name = codeOf s.Kind

        if Configuration.isRuleEnabled fileName code name then
            Some(hint code (messageOf s.Kind) s.Range [ fix s.Range s.OriginalText s.ReplacementText ])
        else
            None)

[<EditorAnalyzer("RedundantSyntax", "Attribute suffix/parens, backticks, hole-free interpolation", HelpBase)>]
let redundantSyntaxEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return redundantSyntaxMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText }

[<CliAnalyzer("RedundantSyntax", "Attribute suffix/parens, backticks, hole-free interpolation", HelpBase)>]
let redundantSyntaxCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async { return redundantSyntaxMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText }

// ---- FR0085 RedundantNew ----

let private redundantNewMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    RedundantNew.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0085"
            $"'new' is conventionally reserved for disposable constructions (the compiler warns the inverse as FS0760); %s{s.TypeName} is not IDisposable, so the keyword is noise."
            s.Range
            [ fix s.Range s.OriginalText "" ])

[<EditorAnalyzer("RedundantNew", "new on non-disposable constructions", HelpBase)>]
let redundantNewEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0085" "RedundantNew" (fun () ->
        whenChecked ctx (redundantNewMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("RedundantNew", "new on non-disposable constructions", HelpBase)>]
let redundantNewCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0085" "RedundantNew" (fun () ->
        redundantNewMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0087 / FR0088 / FR0089 PatternCleanups ----

let private patternCleanupMessages
    (fileName: string)
    (parseTree: ParsedInput)
    (source: ISourceText)
    checkResults
    : Message list =
    let consEnabled = Configuration.isRuleEnabled fileName "FR0087" "ConsListPat"

    let wildEnabled =
        Configuration.isRuleEnabled fileName "FR0088" "RedundantCaseFieldPats"

    let tupleEnabled = Configuration.isRuleEnabled fileName "FR0089" "TupleInList"

    if not (consEnabled || wildEnabled || tupleEnabled) then
        []
    else
        let conses, wilds, tuples = PatternCleanups.find parseTree source checkResults

        [ if consEnabled then
              for s in conses do
                  hint
                      "FR0087"
                      "The pattern `x :: []` is a one-element list; `[ x ]` says so directly."
                      s.Range
                      [ fix s.Range s.OriginalText s.ReplacementText ]
          if wildEnabled then
              for s in wilds do
                  hint
                      "FR0088"
                      $"Every field of %s{s.CaseName} is a wildcard; '%s{s.CaseName} _' matches the same and survives field-count changes."
                      s.Range
                      [ fix s.Range s.OriginalText s.ReplacementText ]
          if tupleEnabled then
              for s in tuples do
                  hint
                      "FR0089"
                      $"This literal holds ONE tuple of %d{s.Elements} elements — ',' builds a tuple, ';' separates elements; if a single-tuple collection is intended, ignore or disable this rule."
                      s.Range
                      [] ]

[<EditorAnalyzer("PatternCleanups", "Cons-of-empty, all-wildcard case fields, tuple-in-list", HelpBase)>]
let patternCleanupsEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async { return whenChecked ctx (patternCleanupMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText) }

[<CliAnalyzer("PatternCleanups", "Cons-of-empty, all-wildcard case fields, tuple-in-list", HelpBase)>]
let patternCleanupsCliAnalyzer (ctx: CliContext) : Async<Message list> =
    async {
        return patternCleanupMessages ctx.FileName ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults
    }

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

// ---- FR0101 IndexedLoop ----

let private indexedLoopMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    IndexedLoop.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0101"
            ($"The index only ever reads '%s{s.CollectionText}.[i]'; iterate '%s{s.CollectionText}' directly.")
            s.Range
            (s.Edits
             |> List.map (fun (r, original, replacement) -> fix r original replacement)))

[<EditorAnalyzer("IndexedLoop", "Index-based loops that only ever index the bound collection", HelpBase)>]
let indexedLoopEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0101" "IndexedLoop" (fun () ->
        indexedLoopMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("IndexedLoop", "Index-based loops that only ever index the bound collection", HelpBase)>]
let indexedLoopCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0101" "IndexedLoop" (fun () ->
        indexedLoopMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0102 ListIndexing ----

let private listIndexingMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    ListIndexing.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0102"
            (sprintf
                "Indexing the F# list '%s' is O(i) per access — inside a loop that is quadratic. Iterate it directly, or convert once with List.toArray if random access is needed."
                s.CollectionText)
            s.Range
            [])

[<EditorAnalyzer("ListIndexing", "Positional indexing into an F# list inside a loop", HelpBase)>]
let listIndexingEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0102" "ListIndexing" (fun () ->
        whenChecked ctx (listIndexingMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("ListIndexing", "Positional indexing into an F# list inside a loop", HelpBase)>]
let listIndexingCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0102" "ListIndexing" (fun () ->
        listIndexingMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)

// ---- FR0103 TypeTestChain ----

let private typeTestChainMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    TypeTestChain.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0103"
            "This if/elif ladder of type tests can be a match with type-test patterns: one test per branch instead of test-plus-cast, and no unsafe :?> left behind."
            s.Range
            [ fix s.Range s.OriginalText s.ReplacementText ])

[<EditorAnalyzer("TypeTestChain", "Type-test if-chains rewritten as match", HelpBase)>]
let typeTestChainEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0103" "TypeTestChain" (fun () ->
        typeTestChainMessages ctx.ParseFileResults.ParseTree ctx.SourceText
        |> commentSafeOnly ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("TypeTestChain", "Type-test if-chains rewritten as match", HelpBase)>]
let typeTestChainCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0103" "TypeTestChain" (fun () ->
        typeTestChainMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0104 RecursiveAppend ----

let private recursiveAppendMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    RecursiveAppend.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0104"
            (sprintf
                "'%s' appends one element to '%s' on every recursive call — the accumulator is copied each step, O(n²) overall. Cons instead ('x :: %s') and List.rev once in the base case, or accumulate into an array when the result is consumed positionally."
                s.FunctionName
                s.AccumulatorName
                s.AccumulatorName)
            s.Range
            [])

[<EditorAnalyzer("RecursiveAppend", "Singleton appends to a recursive accumulator", HelpBase)>]
let recursiveAppendEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0104" "RecursiveAppend" (fun () ->
        recursiveAppendMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("RecursiveAppend", "Singleton appends to a recursive accumulator", HelpBase)>]
let recursiveAppendCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0104" "RecursiveAppend" (fun () ->
        recursiveAppendMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0105 CheckedArithmetic ----

let private checkedArithmeticMessages (parseTree: ParsedInput) (source: ISourceText) : Message list =
    CheckedArithmetic.find parseTree source
    |> List.map (fun s ->
        hint
            "FR0105"
            (sprintf
                "Arithmetic on the near-limit constant %s wraps SILENTLY on overflow — F# operators are unchecked by default. Consider `open Microsoft.FSharp.Core.Operators.Checked` in this scope, a wider type (int64/bigint), or a comment saying the wraparound is intended."
                s.ConstantText)
            s.Range
            [])

[<EditorAnalyzer("CheckedArithmetic", "Unchecked arithmetic on near-limit constants", HelpBase)>]
let checkedArithmeticEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0105" "CheckedArithmetic" (fun () ->
        checkedArithmeticMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

[<CliAnalyzer("CheckedArithmetic", "Unchecked arithmetic on near-limit constants", HelpBase)>]
let checkedArithmeticCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0105" "CheckedArithmetic" (fun () ->
        checkedArithmeticMessages ctx.ParseFileResults.ParseTree ctx.SourceText)

// ---- FR0106 SubstringSpan ----

let private substringSpanMessages (parseTree: ParsedInput) (source: ISourceText) checkResults : Message list =
    SubstringSpan.find parseTree source checkResults
    |> List.map (fun s ->
        hint
            "FR0106"
            (sprintf
                "This Substring allocates a copy that %s immediately discards — AsSpan parses in place (measured 2.6x, allocation-free). The span overload is present in this compilation."
                s.ParserName)
            s.Range
            // a capability fix: on a dual-framework run this may emit an
            // #if NET6_0_OR_GREATER / #else pair instead of the plain swap
            [ CapabilityFix.make source s.Range "Substring" "AsSpan" ])

[<EditorAnalyzer("SubstringSpan", "Parse from a span instead of a Substring copy", HelpBase)>]
let substringSpanEditorAnalyzer (ctx: EditorContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0106" "SubstringSpan" (fun () ->
        whenChecked ctx (substringSpanMessages ctx.ParseFileResults.ParseTree ctx.SourceText))

[<CliAnalyzer("SubstringSpan", "Parse from a span instead of a Substring copy", HelpBase)>]
let substringSpanCliAnalyzer (ctx: CliContext) : Async<Message list> =
    whenEnabled ctx.FileName "FR0106" "SubstringSpan" (fun () ->
        substringSpanMessages ctx.ParseFileResults.ParseTree ctx.SourceText ctx.CheckFileResults)
