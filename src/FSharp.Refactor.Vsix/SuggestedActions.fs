/// Light bulbs: one ISuggestedAction per code action the sidecar offers
/// for the FR diagnostics under the caret. The action titles come from
/// FsAutoComplete, the edits are applied straight to the ITextBuffer.
module FSharp.Refactor.Vsix.SuggestedActions

open System
open System.Collections.Generic
open System.ComponentModel.Composition
open System.Threading.Tasks
open Microsoft.VisualStudio.Imaging.Interop
open Microsoft.VisualStudio.Language.Intellisense
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Utilities
open FSharp.Refactor.Vsix

type FixAction(buffer: ITextBuffer, title: string, edits: (int * int * int * int * string) list) =
    interface ISuggestedAction with
        member _.DisplayText = title
        member _.IconMoniker = Unchecked.defaultof<ImageMoniker>
        member _.IconAutomationText = null
        member _.InputGestureText = null
        member _.HasActionSets = false

        member _.GetActionSetsAsync _ =
            Task.FromResult(Seq.empty: IEnumerable<SuggestedActionSet>)

        member _.HasPreview = false
        member _.GetPreviewAsync _ = Task.FromResult<obj> null

        member _.Invoke(_ct) =
            try
                FsacClient.clientTrace $"invoke '{title}' with {List.length edits} edit(s)"
                let snapshot = buffer.CurrentSnapshot

                use edit = buffer.CreateEdit()

                // bottom-up, so earlier replacements never shift later spans
                for (sl, sc, el, ec, newText) in edits |> List.sortByDescending (fun (sl, sc, _, _, _) -> sl, sc) do
                    if sl < snapshot.LineCount && el < snapshot.LineCount then
                        let startLine = snapshot.GetLineFromLineNumber sl
                        let endLine = snapshot.GetLineFromLineNumber el
                        let startPos = startLine.Start.Position + min sc startLine.Length
                        let endPos = endLine.Start.Position + min ec endLine.Length

                        edit.Replace(Span(startPos, endPos - startPos), newText) |> ignore

                edit.Apply() |> ignore
                FsacClient.clientTrace $"invoke '{title}' applied"
            with ex ->
                FsacClient.clientTrace $"invoke '{title}' FAILED: {ex}"
                reraise ()

        member _.TryGetTelemetryId(telemetryId: byref<Guid>) =
            telemetryId <- Guid.Empty
            false

    interface IDisposable with
        member _.Dispose() = ()

type FrActionsSource(buffer: ITextBuffer, filePath: string) =
    let key = FsacClient.normalizePath filePath
    let changed = Event<EventHandler<EventArgs>, EventArgs>()

    let subscription =
        FsacClient.diagnosticsChanged.Publish.Subscribe(fun changedPath ->
            if String.Equals(changedPath, key, StringComparison.OrdinalIgnoreCase) then
                changed.Trigger(null, EventArgs.Empty))

    let diagsAt (range: SnapshotSpan) =
        ErrorTagger.diagsFor filePath
        |> List.filter (fun d ->
            match ErrorTagger.spanOf range.Snapshot d with
            | Some span -> span.IntersectsWith range
            | None -> false)

    do FsacClient.clientTrace $"actions source created for {filePath}"

    interface ISuggestedActionsSource with
        [<CLIEvent>]
        member _.SuggestedActionsChanged = changed.Publish

        member _.HasSuggestedActionsAsync(_categories, range, _ct) =
            Task.FromResult(not (diagsAt range).IsEmpty)

        member _.GetSuggestedActions(_categories, range, _ct) =
            match diagsAt range with
            | [] -> Seq.empty
            | diags ->
                // sync-over-async on the UI thread with the sidecar's 5s
                // cap — scaffold-grade; the polished version prefetches on
                // caret moves
                let raw = FsacClient.codeActions filePath diags

                FsacClient.clientTrace
                    $"GetSuggestedActions: {List.length diags} diag(s) -> {List.length raw} action(s)"

                // FsAutoComplete titles every analyzer fix "Fix <code>", so
                // a primary and its alternatives render as identical menu
                // entries; append the replacement text so the user can tell
                // which fix is which (upstreaming the title fix to FSAC is
                // the durable version of this)
                let titled =
                    let duplicated =
                        raw
                        |> List.countBy fst
                        |> List.filter (fun (_, n) -> n > 1)
                        |> List.map fst
                        |> Set.ofList

                    raw
                    |> List.map (fun (title, edits) ->
                        match edits with
                        | (_, _, _, _, newText) :: _ when duplicated.Contains title ->
                            let firstLine =
                                let t = newText.Trim()
                                let i = t.IndexOfAny [| '\r'; '\n' |]
                                let line = if i >= 0 then t.Substring(0, i) else t

                                if line.Length > 50 then
                                    line.Substring(0, 47) + "..."
                                else
                                    line

                            $"{title} → {firstLine}", edits
                        | _ -> title, edits)

                let actions =
                    titled
                    |> List.map (fun (title, edits) -> FixAction(buffer, title, edits) :> ISuggestedAction)

                if actions.IsEmpty then
                    Seq.empty
                else
                    [ SuggestedActionSet(PredefinedSuggestedActionCategoryNames.CodeFix, actions, "FSharp.Refactor") ]
                    :> seq<_>

        member _.TryGetTelemetryId(telemetryId: byref<Guid>) =
            telemetryId <- Guid.Empty
            false

    interface IDisposable with
        member _.Dispose() = subscription.Dispose()

[<Export(typeof<ISuggestedActionsSourceProvider>)>]
[<Name "FSharp.Refactor Suggested Actions">]
[<ContentType "F#">]
type FrActionsSourceProvider() =
    interface ISuggestedActionsSourceProvider with
        member _.CreateSuggestedActionsSource(_view: ITextView, buffer: ITextBuffer) =
            match BufferSessions.ensureFor buffer with
            | Some session -> new FrActionsSource(buffer, session.FilePath) :> ISuggestedActionsSource
            | None -> null
