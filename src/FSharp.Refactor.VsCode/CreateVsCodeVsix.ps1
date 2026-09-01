# Builds and packages the VS Code companion extension. Cross-platform
# (pwsh on Linux CI works): stages both analyzer SDK builds, stamps the
# version from Directory.Build.props, compiles the TypeScript, and runs
# vsce package.
#
#     pwsh -File CreateVsCodeVsix.ps1
#     code --install-extension artifacts/fsharp-refactor-<version>.vsix
param([string]$Configuration = "Release")

$ErrorActionPreference = "Stop"
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$repo = Resolve-Path (Join-Path $here "../..")

dotnet build (Join-Path $repo "src/FSharp.Refactor.Analyzers") -c $Configuration
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

$analyzerDir = Join-Path $here "analyzers"
if (Test-Path $analyzerDir) { Remove-Item -Recurse -Force $analyzerDir }
New-Item -ItemType Directory -Force $analyzerDir | Out-Null
Copy-Item (Join-Path $repo "src/FSharp.Refactor.Analyzers/bin/$Configuration/net8.0/FSharp.Refactor.Analyzers.dll") $analyzerDir
Copy-Item (Join-Path $repo "src/FSharp.Refactor.Analyzers.Ionide/bin/$Configuration/net8.0/FSharp.Refactor.Analyzers.Ionide.dll") $analyzerDir
Copy-Item (Join-Path $repo "icon.png") $here -Force
Copy-Item (Join-Path $repo "LICENSE") $here -Force

# ONE version source, same as the vsix and the nupkgs
$repoVersion = [regex]::Match(
    [IO.File]::ReadAllText((Join-Path $repo "Directory.Build.props")),
    '<Version>([^<]+)</Version>').Groups[1].Value
$packageJsonPath = Join-Path $here "package.json"
$packageJson = [IO.File]::ReadAllText($packageJsonPath)
$packageJson = $packageJson -replace '("version":\s*")[^"]+(")', "`${1}$repoVersion`${2}"
[IO.File]::WriteAllText($packageJsonPath, $packageJson)
Write-Host "Stamped VS Code extension version $repoVersion"

Push-Location $here
try {
    npm install --no-audit --no-fund
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    npm run compile
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

    $artifacts = Join-Path $here "artifacts"
    New-Item -ItemType Directory -Force $artifacts | Out-Null
    npx vsce package --out (Join-Path $artifacts "fsharp-refactor-$repoVersion.vsix")
    if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
    Write-Host "Created $(Join-Path $artifacts "fsharp-refactor-$repoVersion.vsix")"
}
finally {
    Pop-Location
}
