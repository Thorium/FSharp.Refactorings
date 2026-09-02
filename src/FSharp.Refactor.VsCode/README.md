# FSharp.Refactor for VS Code


[Get it from the Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=TuomasHietanen.fsharp-refactor-vscode)


140 functional refactoring hints with one-click quick fixes for F#,
delivered through Ionide.

VS Code has no analyzer concept of its own — F# analyzers load through
Ionide → FsAutoComplete → FSharp.Analyzers.SDK, from the directories the
`FSharp.analyzersPath` setting names. This extension bundles the
FSharp.Refactor analyzer assemblies (both SDK builds: FsAutoComplete
loads the one matching its own SDK version and skips the other) and, on
first activation, **asks** to append its analyzers directory to that
setting and turn `FSharp.enableAnalyzers` on. Decline and it stays out of
your settings; the `FSharp.Refactor: Wire analyzers into Ionide` command
re-offers it any time, and the `Remove` command undoes it.

After wiring and a window reload, open any F# file: suggestions appear as
`Hint`-severity squiggles with `FR`-prefixed codes, each carrying a
light-bulb quick fix (`Ctrl+.`).

Alternative without this extension: reference the
`FSharp.Refactor.Analyzers` NuGet package and point `FSharp.analyzersPath`
at the restored package — per-project instead of global; see the
[project README](https://github.com/Thorium/fsharp-refactor).

## Building

```
pwsh -File CreateVsCodeVsix.ps1
code --install-extension artifacts/fsharp-refactor-<version>.vsix
```

The version is stamped from the repo's `Directory.Build.props`, the same
single source as the NuGet packages and the Visual Studio extension.

## The durable fix

Mutating user settings is a workaround for Ionide having no extension
point for third-party analyzer directories. The right long-term shape is
an Ionide `contributes`-style API where an extension declares its
analyzer paths and Ionide collects them — worth proposing upstream; this
extension then shrinks to a manifest entry.
