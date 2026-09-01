/// Wires an ITextBuffer to the sidecar's document sync: didOpen on first
/// sight, full-text didChange on edits (debounced). Created lazily by the
/// tagger provider, one per buffer, tracked by buffer properties.
module FSharp.Refactor.Vsix.BufferSessions

open System
open System.IO
open System.Threading
open Microsoft.VisualStudio.Text
open FSharp.Refactor.Vsix

type BufferSession(buffer: ITextBuffer, filePath: string) =
    let mutable version = 1
    let mutable pendingTimer: Timer option = None

    let rootDir =
        // FSAC discovers the workspace itself (AutomaticWorkspaceInit);
        // rooting at the directory that holds a sln/fsproj above the file
        // gives it the right place to look
        let rec findRoot (dir: DirectoryInfo) =
            if isNull dir then
                Path.GetDirectoryName filePath
            elif
                Directory.EnumerateFiles(dir.FullName)
                |> Seq.exists (fun f -> f.EndsWith ".sln" || f.EndsWith ".slnx" || f.EndsWith ".fsproj")
            then
                dir.FullName
            else
                findRoot dir.Parent

        findRoot (DirectoryInfo(Path.GetDirectoryName filePath))

    let sendChange () =
        version <- version + 1
        FsacClient.notifyChanged filePath version (buffer.CurrentSnapshot.GetText())

    do
        // OFF the UI thread: this constructor runs inside tagger creation,
        // and the first session spawns a process and waits for its LSP
        // initialize — synchronously that froze Visual Studio for the
        // whole handshake (the responsiveness banner fired at 8s, live)
        let openText = buffer.CurrentSnapshot.GetText()

        System.Threading.Tasks.Task.Run(fun () ->
            match FsacClient.ensure rootDir with
            | Some _ -> FsacClient.notifyOpened filePath openText
            | None -> ())
        |> ignore

        buffer.Changed.Add(fun _ ->
            // debounce: FSAC rechecks per didChange; typing bursts collapse
            match pendingTimer with
            | Some t -> t.Dispose()
            | None -> ()

            pendingTimer <- Some(new Timer((fun _ -> sendChange ()), null, 500, Timeout.Infinite)))

    member _.FilePath = filePath

/// One session per buffer, created on demand.
let ensureFor (buffer: ITextBuffer) : BufferSession option =
    match buffer.Properties.TryGetProperty<BufferSession>(typeof<BufferSession>) with
    | true, s -> Some s
    | _ ->
        match buffer.Properties.TryGetProperty<ITextDocument>(typeof<ITextDocument>) with
        | true, doc when not (String.IsNullOrEmpty doc.FilePath) ->
            let s = BufferSession(buffer, doc.FilePath)
            buffer.Properties.AddProperty(typeof<BufferSession>, s)
            Some s
        | _ -> None
