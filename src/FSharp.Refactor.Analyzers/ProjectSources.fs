/// Parse trees for OTHER files of the project under analysis.
///
/// Cross-file migrations (internal-visibility FR0069/FR0093, taskify)
/// classify every use of a symbol; uses in other files need that file's
/// parse tree and source. The HOST decides whether that is possible:
/// the CLI configures a parser callback per compilation (it owns the
/// checker and the project options), the editor leaves it unconfigured —
/// rules then degrade to their file-local behavior.
///
/// Results are cached per (path, defines-fingerprint set by the host at
/// configure time); the host reconfigures per compilation, which resets
/// the cache — a different framework's defines produce a different tree.
module FSharp.Refactor.ProjectSources

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

/// Does the assembly OPEN its internals to friends? InternalsVisibleTo
/// makes "internal ⇒ every caller is in this project's symbol uses"
/// FALSE — the test assembly's call sites are invisible to the scan and
/// to the verification build — so every cross-file internal migration
/// must stand down. Fail-safe: an unreadable signature counts as having
/// friends.
let hasInternalsVisibleTo (projectCheck: FSharpCheckProjectResults) =
    try
        projectCheck.AssemblySignature.Attributes
        |> Seq.exists (fun a ->
            try
                a.AttributeType.DisplayName.Contains "InternalsVisibleTo"
            with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                true)
    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
        true

let mutable private parser: (string -> (ParsedInput * ISourceText) option) option =
    None

let private cache =
    System.Collections.Concurrent.ConcurrentDictionary<string, (ParsedInput * ISourceText) option>()

/// Install the host's parser for the CURRENT compilation and reset the
/// cache. Pass None to uninstall (editor hosts never install one).
let configure (parse: (string -> (ParsedInput * ISourceText) option) option) =
    parser <- parse
    cache.Clear()

/// Drop cached trees without changing the parser. The host MUST call this
/// between fix-apply passes: a pass-1 edit to a sibling file would
/// otherwise leave pass-2 computing edits against the stale tree.
let invalidate () = cache.Clear()

/// Whether cross-file classification is possible in this host.
let available () = parser.IsSome

/// The parse tree and source of a project file, by full path.
let tryParse (path: string) : (ParsedInput * ISourceText) option =
    match parser with
    | None -> None
    | Some parse ->
        cache.GetOrAdd(
            System.IO.Path.GetFullPath(path).ToLowerInvariant(),
            fun _ ->
                try
                    parse path
                with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                    None
        )
