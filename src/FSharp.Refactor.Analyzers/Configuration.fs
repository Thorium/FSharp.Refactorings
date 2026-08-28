/// Optional per-repository configuration, fsharplint.json-style.
///
/// A file named `fsharprefactor.json` is searched upward from the
/// analyzed file's directory; the nearest one wins. Rules are keyed by code
/// or analyzer name (case-insensitive) and every rule defaults to enabled:
///
///     {
///       "rules": {
///         "FR0001": false,
///         "conversionMove": { "enabled": false }
///       }
///     }
///
/// The `rules` wrapper is optional — rule keys may also sit at the root.
/// A malformed or unreadable file fails open (everything enabled) so a bad
/// config can never break the user's editor; unknown keys are ignored.
///
/// Lookups are cached: directory discovery is revalidated after a short
/// interval and the parsed file is reloaded when its timestamp changes, so
/// config edits are picked up without restarting the editor.
module FSharp.Refactor.Configuration

open System
open System.Collections.Concurrent
open System.IO
open System.Text.Json

[<Literal>]
let ConfigFileName = "fsharprefactor.json"

/// Parse the config text into a rule-key -> enabled map (keys lowercased).
/// Pure and total: malformed input yields an empty map (fail open).
let parse (json: string) : Map<string, bool> =
    try
        let options =
            JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

        use doc = JsonDocument.Parse(json, options)
        let root = doc.RootElement

        let rulesElement =
            match root.TryGetProperty "rules" with
            | true, rules when rules.ValueKind = JsonValueKind.Object -> rules
            | _ -> root

        rulesElement.EnumerateObject()
        |> Seq.choose (fun property ->
            let enabled =
                match property.Value.ValueKind with
                | JsonValueKind.True -> Some true
                | JsonValueKind.False -> Some false
                | JsonValueKind.Object ->
                    match property.Value.TryGetProperty "enabled" with
                    | true, e when e.ValueKind = JsonValueKind.True -> Some true
                    | true, e when e.ValueKind = JsonValueKind.False -> Some false
                    | _ -> None
                | _ -> None

            enabled |> Option.map (fun e -> property.Name.ToLowerInvariant(), e))
        |> Map.ofSeq
    with
    | :? JsonException
    | :? InvalidOperationException -> Map.empty

/// Is the rule enabled in a parsed rule map? An explicit code entry wins over
/// a name entry; absent rules are enabled.
let isEnabledIn (rules: Map<string, bool>) (code: string) (analyzerName: string) : bool =
    rules.TryFind(code.ToLowerInvariant())
    |> Option.orElseWith (fun () -> rules.TryFind(analyzerName.ToLowerInvariant()))
    |> Option.defaultValue true

/// Walk up from `directory` looking for the config file, stopping at the
/// repository root (the first directory holding .git) — a stray
/// fsharprefactor.json above the checkout must not silently reconfigure
/// every repository beneath it.
[<TailCall>]
let rec private findConfigUpward (directory: string) : string option =
    if String.IsNullOrEmpty directory then
        None
    else
        let candidate = Path.Combine(directory, ConfigFileName)

        if File.Exists candidate then
            Some candidate
        elif
            Directory.Exists(Path.Combine(directory, ".git"))
            || File.Exists(Path.Combine(directory, ".git"))
        then
            None
        else
            match Path.GetDirectoryName directory with
            | null -> None
            | parent -> findConfigUpward parent

// A few long-lived caches (per our own guidance: few static dictionaries,
// accessed via GetOrAdd/AddOrUpdate only).
let private discoveryRevalidateAfter = TimeSpan.FromSeconds 5.0

let private discoveryCache =
    ConcurrentDictionary<string, DateTime * string option>()

/// Additional hint-engine rules from the config's `hints.add` array
/// (fsharplint-style). Pure and total: anything malformed yields an empty list.
let parseHints (json: string) : string list =
    try
        let options =
            JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

        use doc = JsonDocument.Parse(json, options)

        match doc.RootElement.TryGetProperty "hints" with
        | true, hints when hints.ValueKind = JsonValueKind.Object ->
            match hints.TryGetProperty "add" with
            | true, add when add.ValueKind = JsonValueKind.Array ->
                add.EnumerateArray()
                |> Seq.choose (fun item ->
                    if item.ValueKind = JsonValueKind.String then
                        Some(item.GetString())
                    else
                        None)
                |> List.ofSeq
            | _ -> []
        | _ -> []
    with
    | :? JsonException
    | :? InvalidOperationException -> []

/// The parsed content of one config file.
type ConfigData =
    { Rules: Map<string, bool>
      Hints: string list }

let private emptyConfig = { Rules = Map.empty; Hints = [] }

let private parseCache = ConcurrentDictionary<string, DateTime * ConfigData>()

/// The effective configuration for a file being analyzed. Discovery is
/// memoized per directory with a short revalidation window; the parsed
/// content is reloaded when the config file's timestamp changes.
let configFor (analyzedFile: string) : ConfigData =
    let directory =
        // invalid path characters mean no config directory to search
        try
            Path.GetDirectoryName(analyzedFile: string)
        with
        | :? ArgumentException
        | :? PathTooLongException -> null

    if String.IsNullOrEmpty directory then
        emptyConfig
    else
        let _, configPath =
            discoveryCache.AddOrUpdate(
                directory,
                (fun dir -> DateTime.UtcNow, findConfigUpward dir),
                (fun dir (checkedAt, cached) ->
                    if DateTime.UtcNow - checkedAt > discoveryRevalidateAfter then
                        DateTime.UtcNow, findConfigUpward dir
                    else
                        checkedAt, cached)
            )

        match configPath with
        | None -> emptyConfig
        | Some path ->
            // a vanished or unreadable config reads as never-written, which
            // forces a re-read attempt on the next call
            let lastWrite =
                try
                    File.GetLastWriteTimeUtc path
                with
                | :? IOException
                | :? UnauthorizedAccessException
                | :? ArgumentException -> DateTime.MinValue

            let readCurrent (p: string) =
                // a config deleted or locked mid-read means no config
                let content =
                    try
                        File.ReadAllText p
                    with
                    | :? IOException
                    | :? UnauthorizedAccessException -> ""

                lastWrite,
                { Rules = parse content
                  Hints = parseHints content }

            let _, config =
                parseCache.AddOrUpdate(
                    path,
                    readCurrent,
                    (fun p (cachedWrite, cached) ->
                        if cachedWrite <> lastWrite then
                            readCurrent p
                        else
                            cachedWrite, cached)
                )

            config

/// The effective rule map for a file being analyzed.
let rulesFor (analyzedFile: string) : Map<string, bool> = (configFor analyzedFile).Rules

/// Extra hint-engine rules configured for a file being analyzed.
let hintsFor (analyzedFile: string) : string list = (configFor analyzedFile).Hints

/// The single entry point the analyzers use: is this rule enabled for this file?
/// Build-generated sources (AssemblyInfo.fs, AssemblyAttributes.fs under
/// obj/) are not the user's code; no rule has business flagging them.
let isGeneratedFile (analyzedFile: string) =
    analyzedFile.Contains @"\obj\" || analyzedFile.Contains "/obj/"

let isRuleEnabled (analyzedFile: string) (code: string) (analyzerName: string) : bool =
    not (isGeneratedFile analyzedFile)
    && isEnabledIn (rulesFor analyzedFile) code analyzerName
