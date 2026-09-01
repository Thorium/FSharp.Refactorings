# FSharp.Refactor for Visual Studio (classic VSIX)

[Get it from the Visual Studio Marketplace](https://marketplace.visualstudio.com/items?itemName=TuomasHietanen.fSharp-refactor)


Squiggles and light-bulb quick fixes from the FSharp.Refactor analyzers
inside full Visual Studio, using the classic in-proc MEF editor surfaces:

- `ErrorTagger.fs` — `IViewTaggerProvider`/`ITagger<IErrorTag>` on content
  type `F#`, rendering each FR diagnostic with the editor's hinted-
  suggestion style.
- `SuggestedActions.fs` — `ISuggestedActionsSourceProvider`/
  `ISuggestedAction` (the light bulb), titles and edits straight from the
  sidecar's code actions.
- `FsacClient.fs` + `Lsp.fs` — an **FsAutoComplete sidecar** spoken to
  over LSP stdio. Out-of-process on purpose: VS's own F# tools load their
  own FSharp.Compiler.Service in-proc, and loading ours beside it is the
  assembly-binding wound Visual F# Power Tools kept reopening. We consume
  exactly two things: `publishDiagnostics` (filtered to FR codes — VS
  already shows compiler errors) and `textDocument/codeAction`.
- `BufferSessions.fs` — didOpen/didChange sync per `ITextBuffer`,
  debounced.

## Build + try

```bash
powershell -File src/FSharp.Refactor.Vsix/CreateVsix.ps1
```

then install into the experimental instance and start it:

```bash
VSIXInstaller /rootSuffix:Exp src/FSharp.Refactor.Vsix/artifacts/FSharp.Refactor.vsix
devenv /rootSuffix Exp
```

Open an F# project (`C:\git\refactortest` is a ready trigger corpus) and
watch for dotted suggestion underlines; `Ctrl+.` on one shows the fixes.

FsAutoComplete is located as the global dotnet tool
(`%USERPROFILE%\.dotnet\tools\fsautocomplete.exe`, install with
`dotnet tool install -g fsautocomplete`), or from an `fsac\` folder beside
the extension dll if you bundle one. The bundled `analyzers\` folder
carries both SDK builds of the analyzers; FSAC loads the one its
FSharp.Analyzers.SDK version pairs with and log-skips the other.

## Status

**Working end to end, verified live in VS 2026** (squiggles, light bulb,
fixes applied). The marketplace listing text lives in
`MarketplaceOverview.md`; `publishManifest.json` + the CI `vsix` job
handle publishing on version tags (needs the `VS_MARKETPLACE_PAT`
secret).

Fast dev loop, no reinstall: build, then copy
`bin\Release\net48\FSharp.Refactor.Vsix.dll` over the installed copy
under `%LOCALAPPDATA%\Microsoft\VisualStudio\18.0_*Exp\Extensions\<random>\`
and delete that instance's `ComponentModelCache` (required when MEF
exports changed). Everything traces to `%TEMP%\FSharpRefactor.Vsix.log`.

Polish backlog (deliberately V1):

- `GetSuggestedActions` blocks the UI thread on the sidecar (5s cap)
  instead of prefetching on caret moves.
- The sidecar roots at the nearest directory with a sln/slnx/fsproj above
  the first opened file, and relies on FSAC's `AutomaticWorkspaceInit`.
- Fix edits apply to the buffer without a preview pane.
- No options page: analyzer path and FSAC location are convention-only;
  FSAC is located as the global dotnet tool, not yet bundled.
