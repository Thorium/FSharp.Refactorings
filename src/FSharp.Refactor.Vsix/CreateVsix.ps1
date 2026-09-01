# Builds and packages the Visual Studio extension. A .vsix is an OPC zip:
# payload + extension.vsixmanifest + [Content_Types].xml — assembled by
# hand here because the VSSDK.BuildTools packaging targets predate
# SDK-style fsproj and fight it.
#
#     powershell -File CreateVsix.ps1
#     VSIXInstaller /rootSuffix:Exp artifacts\FSharp.Refactor.vsix   # test instance
#     VSIXInstaller artifacts\FSharp.Refactor.vsix                   # real VS
param([string]$Configuration = "Release")

$ErrorActionPreference = "Stop"
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$repo = Resolve-Path (Join-Path $here "..\..")

dotnet build $here -c $Configuration
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

# the analyzers the sidecar loads: build both SDK flavors
dotnet build (Join-Path $repo "src\FSharp.Refactor.Analyzers") -c $Configuration
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

$staging = Join-Path $here "obj\vsix-staging"
if (Test-Path $staging) { Remove-Item -Recurse -Force $staging }
New-Item -ItemType Directory -Force $staging | Out-Null

$bin = Join-Path $here "bin\$Configuration\net48"

# only OUR payload — the editor/MEF assemblies come from Visual Studio
foreach ($dll in "FSharp.Refactor.Vsix.dll", "FSharp.Core.dll", "Newtonsoft.Json.dll") {
    Copy-Item (Join-Path $bin $dll) $staging
}

# analyzers for the FSAC sidecar (it skips the SDK flavor it cannot load)
$analyzerDir = Join-Path $staging "analyzers"
New-Item -ItemType Directory -Force $analyzerDir | Out-Null
Copy-Item (Join-Path $repo "src\FSharp.Refactor.Analyzers\bin\$Configuration\net8.0\FSharp.Refactor.Analyzers.dll") $analyzerDir
Copy-Item (Join-Path $repo "src\FSharp.Refactor.Analyzers.Ionide\bin\$Configuration\net8.0\FSharp.Refactor.Analyzers.Ionide.dll") $analyzerDir

Copy-Item (Join-Path $repo "LICENSE") $staging
Copy-Item (Join-Path $repo "icon.png") $staging

# the packaged manifest is the PROCESSED form: VS's own build strips the
# design-time d: namespace before packaging, and the installer's reader
# rejects manifests that still carry it
$manifest = [IO.File]::ReadAllText((Join-Path $here "source.extension.vsixmanifest"))
$manifest = $manifest -replace '\s+xmlns:d="[^"]*"', ''
$manifest = $manifest -replace '\s+d:\w+="[^"]*"', ''
$manifest = $manifest -replace '<\?xml[^>]*\?>\s*', ''

# ONE version source: the extension's user-visible version (Manage
# Extensions, the marketplace listing, upgrade detection) is stamped from
# Directory.Build.props, so a release bump cannot forget the vsix
$repoVersion = [regex]::Match(
    [IO.File]::ReadAllText((Join-Path $repo "Directory.Build.props")),
    '<Version>([^<]+)</Version>').Groups[1].Value
if ($repoVersion) {
    $manifest = $manifest -replace '(<Identity [^>]*Version=")[^"]+(")', "`${1}$repoVersion`${2}"
    Write-Host "Stamped vsix version $repoVersion"
}

[IO.File]::WriteAllText((Join-Path $staging "extension.vsixmanifest"), $manifest)

# VSIX v3 servicing files: the modern installer engine refuses a package
# without manifest.json + catalog.json (shapes templated from a working
# marketplace vsix; sha256 may be null)
$vsixId = [regex]::Match($manifest, 'Identity Id="([^"]+)"').Groups[1].Value
$version = [regex]::Match($manifest, 'Identity Id="[^"]+" Version="([^"]+)"').Groups[1].Value
$displayName = [regex]::Match($manifest, '<DisplayName>([^<]+)</DisplayName>').Groups[1].Value

$payloadFiles = Get-ChildItem -LiteralPath $staging -Recurse -File
$totalSize = ($payloadFiles | Measure-Object Length -Sum).Sum

$fileEntries = ($payloadFiles | ForEach-Object {
        $rel = '/' + $_.FullName.Substring($staging.Length + 1).Replace('\', '/')
        '{"fileName":"' + $rel + '","sha256":null}'
    }) -join ','

$manifestJson = '{"id":"' + $vsixId + '","version":"' + $version + '","type":"Vsix","vsixId":"' + $vsixId +
'","extensionDir":"[installdir]\\Common7\\IDE\\Extensions\\fsrefact.vs","files":[' + $fileEntries +
'],"installSizes":{"targetDrive":' + $totalSize + '},"dependencies":{"Microsoft.VisualStudio.Component.CoreEditor":"17.0"}}'
[IO.File]::WriteAllText((Join-Path $staging "manifest.json"), $manifestJson)

$catalogJson = '{"manifestVersion":"1.1","info":{"id":"' + $vsixId + ',version=' + $version +
'","manifestType":"Extension"},"packages":[{"id":"Component.' + $vsixId + '","version":"' + $version +
'","type":"Component","extension":true,"dependencies":{"' + $vsixId + '":"' + $version +
'","Microsoft.VisualStudio.Component.CoreEditor":"17.0"},"localizedResources":[{"language":"en-US","title":"' +
$displayName + '","description":"' + $displayName + '"}]},{"id":"' + $vsixId + '","version":"' + $version +
'","type":"Vsix","payloads":[{"fileName":"FSharp.Refactor.vsix","size":' + $totalSize + '}],"vsixId":"' + $vsixId +
'","extensionDir":"[installdir]\\Common7\\IDE\\Extensions\\fsrefact.vs","installSizes":{"targetDrive":' + $totalSize + '}}]}'
[IO.File]::WriteAllText((Join-Path $staging "catalog.json"), $catalogJson)

# OPC forbids empty-Extension Defaults; the extensionless LICENSE gets an
# Override part entry instead
@'
<?xml version="1.0" encoding="utf-8"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="dll" ContentType="application/octet-stream" />
  <Default Extension="vsixmanifest" ContentType="text/xml" />
  <Default Extension="json" ContentType="application/json" />
  <Default Extension="png" ContentType="application/octet-stream" />
  <Override PartName="/LICENSE" ContentType="text/plain" />
</Types>
'@ | Out-File -Encoding utf8 -LiteralPath (Join-Path $staging "[Content_Types].xml")

$artifacts = Join-Path $here "artifacts"
New-Item -ItemType Directory -Force $artifacts | Out-Null
$vsix = Join-Path $artifacts "FSharp.Refactor.vsix"
if (Test-Path $vsix) { Remove-Item -Force $vsix }

# manual zip: .NET Framework's CreateFromDirectory writes backslash entry
# names, which the OPC reader silently drops — part names must use '/'
Add-Type -AssemblyName System.IO.Compression
Add-Type -AssemblyName System.IO.Compression.FileSystem
$archive = [System.IO.Compression.ZipFile]::Open($vsix, [System.IO.Compression.ZipArchiveMode]::Create)
try {
    Get-ChildItem -LiteralPath $staging -Recurse -File | ForEach-Object {
        $rel = $_.FullName.Substring($staging.Length + 1).Replace('\', '/')
        [System.IO.Compression.ZipFileExtensions]::CreateEntryFromFile($archive, $_.FullName, $rel) | Out-Null
    }
}
finally {
    $archive.Dispose()
}

Write-Host "Created $vsix"
