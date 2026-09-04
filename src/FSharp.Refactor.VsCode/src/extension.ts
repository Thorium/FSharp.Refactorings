// VS Code has no analyzer concept of its own: F# analyzers load through
// Ionide -> FsAutoComplete -> FSharp.Analyzers.SDK, from the directories
// the `FSharp.analyzersPath` setting names. This extension bundles the
// FSharp.Refactor analyzer assemblies (BOTH SDK builds — FSAC loads the
// one matching its own SDK version and skips the other) and, with the
// user's consent, appends its own analyzers directory to that setting.
import * as cp from 'child_process';
import * as vscode from 'vscode';

const SECTION = 'FSharp';
// entries written by ANY version of this extension carry the extension id
// in their path (…/thorium.fsharp-refactor-<version>/analyzers), so stale
// versions can be recognized and replaced on update
const PATH_MARKER = '.fsharp-refactor-';
const DECLINED_KEY = 'fsharpRefactor.wireDeclined';

function analyzersDir(context: vscode.ExtensionContext): string {
    return vscode.Uri.joinPath(context.extensionUri, 'analyzers').fsPath;
}

async function promptReload(message: string): Promise<void> {
    const pick = await vscode.window.showInformationMessage(message, 'Reload Window');
    if (pick === 'Reload Window') {
        await vscode.commands.executeCommand('workbench.action.reloadWindow');
    }
}

async function wire(context: vscode.ExtensionContext, interactive: boolean): Promise<void> {
    const config = vscode.workspace.getConfiguration(SECTION);
    const dir = analyzersDir(context);
    const current = config.get<string[]>('analyzersPath') ?? [];

    // replace entries from older versions of this extension, keep the rest
    const kept = current.filter(p => !p.includes(PATH_MARKER));
    const wired = current.includes(dir);
    const enabled = config.get<boolean>('enableAnalyzers') ?? false;

    if (wired && enabled) {
        if (interactive) {
            vscode.window.showInformationMessage('FSharp.Refactor analyzers are already wired into Ionide.');
        }
        return;
    }

    await config.update('analyzersPath', [...kept, dir], vscode.ConfigurationTarget.Global);
    if (!enabled) {
        await config.update('enableAnalyzers', true, vscode.ConfigurationTarget.Global);
    }

    await context.globalState.update(DECLINED_KEY, undefined);
    await promptReload('FSharp.Refactor analyzers wired into Ionide. Reload to activate?');
}

async function unwire(context: vscode.ExtensionContext): Promise<void> {
    const config = vscode.workspace.getConfiguration(SECTION);
    const current = config.get<string[]>('analyzersPath') ?? [];
    const kept = current.filter(p => !p.includes(PATH_MARKER));

    if (kept.length === current.length) {
        vscode.window.showInformationMessage('FSharp.Refactor analyzers were not wired.');
        return;
    }

    await config.update('analyzersPath', kept.length > 0 ? kept : undefined, vscode.ConfigurationTarget.Global);
    // leave enableAnalyzers alone: the user may have other analyzers
    await context.globalState.update(DECLINED_KEY, true);
    await promptReload('FSharp.Refactor analyzers removed from Ionide settings. Reload to apply?');
}

// the extension shipped under this id before it moved publishers; both
// installed side by side register the same commands, and the second one
// to activate threw in registerCommand — before it could re-point the
// analyzers setting, which is how a machine kept running the old build
const PREVIOUS_ID = 'thorium.fsharp-refactor';

async function retirePrevious(): Promise<void> {
    const previous = vscode.extensions.getExtension(PREVIOUS_ID);
    if (!previous) {
        return;
    }

    const pick = await vscode.window.showWarningMessage(
        `FSharp.Refactor: an older copy (${PREVIOUS_ID}) is installed beside this one and competes for the same settings. Uninstall it?`,
        'Uninstall old copy',
        'Keep both'
    );

    if (pick === 'Uninstall old copy') {
        try {
            await vscode.commands.executeCommand('workbench.extensions.uninstallExtension', PREVIOUS_ID);
            await promptReload('Old FSharp.Refactor copy removed. Reload to finish?');
        } catch (error) {
            vscode.window.showErrorMessage(`FSharp.Refactor: could not uninstall ${PREVIOUS_ID}: ${String(error)}`);
        }
    }
}

// ---- the apply tool: fsharp-refactor as a dotnet global tool ----

const TOOL = 'fsharp-refactor';

/// The tool's version, or undefined when it is not on the PATH.
function toolVersion(): Promise<string | undefined> {
    return new Promise(resolve => {
        cp.execFile(TOOL, ['--version'], { timeout: 15000, shell: true }, (error, stdout) => {
            if (error) {
                resolve(undefined);
            } else {
                resolve(String(stdout).trim());
            }
        });
    });
}

async function exists(uri: vscode.Uri): Promise<boolean> {
    try {
        await vscode.workspace.fs.stat(uri);
        return true;
    } catch {
        return false;
    }
}

let terminal: vscode.Terminal | undefined;

function toolTerminal(): vscode.Terminal {
    if (!terminal || terminal.exitStatus) {
        terminal = vscode.window.createTerminal(TOOL);
    }
    return terminal;
}

/// Everything a "why do I see no hints?" question needs, in one place:
/// the extension, its analyzers, the wiring, Ionide and its FSAC, the tool.
async function status(context: vscode.ExtensionContext): Promise<void> {
    const pkg = context.extension.packageJSON as {
        version?: string;
        fsharpRefactorBuild?: { analyzers?: string; built?: string };
    };
    const config = vscode.workspace.getConfiguration(SECTION);
    const dir = analyzersDir(context);
    const paths = config.get<string[]>('analyzersPath') ?? [];
    const enabled = config.get<boolean>('enableAnalyzers') ?? false;
    const ionide = vscode.extensions.getExtension('ionide.ionide-fsharp');
    const ionideVersion = (ionide?.packageJSON as { version?: string } | undefined)?.version;
    const fsacSetting = config.get<string>('fsac.netCoreDllPath');
    const fsacBundled = ionide ? vscode.Uri.joinPath(ionide.extensionUri, 'bin', 'fsautocomplete.dll') : undefined;
    const fsacPath = fsacSetting && fsacSetting.length > 0
        ? fsacSetting
        : fsacBundled && (await exists(fsacBundled)) ? fsacBundled.fsPath : '(not found)';
    const stale = paths.filter(p => p.includes(PATH_MARKER) && p !== dir);
    const tool = await toolVersion();

    const lines = [
        `FSharp.Refactor extension ${pkg.version ?? '?'} (${context.extension.id})`,
        `Analyzers ${pkg.fsharpRefactorBuild?.analyzers ?? '?'}, built ${pkg.fsharpRefactorBuild?.built || '?'}`,
        `  ${dir} ${(await exists(vscode.Uri.file(dir))) ? '(present)' : '(MISSING)'}`,
        `Wired into Ionide: ${paths.includes(dir) ? 'yes' : 'NO'}; FSharp.enableAnalyzers: ${enabled}`,
        ...(stale.length > 0 ? [`  stale entries from older versions: ${stale.join(', ')}`] : []),
        `Ionide ${ionideVersion ?? '(not installed)'}; FsAutoComplete: ${fsacPath}`,
        `Apply tool (${TOOL}): ${tool ?? 'not installed — dotnet tool install -g fsharp-refactor'}`,
        `Older copy (${PREVIOUS_ID}): ${vscode.extensions.getExtension(PREVIOUS_ID) ? 'STILL INSTALLED' : 'not installed'}`,
    ];

    const channel = vscode.window.createOutputChannel('FSharp.Refactor');
    channel.clear();
    for (const line of lines) {
        channel.appendLine(line);
    }
    channel.show(true);

    const problems = [
        ...(paths.includes(dir) && enabled ? [] : ['analyzers not wired']),
        ...(stale.length > 0 ? ['stale entries'] : []),
        ...(tool ? [] : ['tool not installed']),
    ];
    vscode.window.showInformationMessage(
        problems.length === 0
            ? `FSharp.Refactor ${pkg.version ?? ''}: everything wired (details in the Output panel).`
            : `FSharp.Refactor ${pkg.version ?? ''}: ${problems.join(', ')} (details in the Output panel).`
    );
}

/// Run the apply tool on a solution or project of this workspace, in the
/// integrated terminal: report only by default, the real thing on request.
async function run(): Promise<void> {
    const folders = vscode.workspace.workspaceFolders ?? [];
    if (folders.length === 0) {
        vscode.window.showWarningMessage('FSharp.Refactor: open a folder or workspace first.');
        return;
    }

    const found = await vscode.workspace.findFiles(
        '**/*.{sln,slnx,fsproj}',
        '**/{node_modules,bin,obj,packages,paket-files,.git}/**',
        100
    );
    if (found.length === 0) {
        vscode.window.showWarningMessage('FSharp.Refactor: no .sln, .slnx or .fsproj in this workspace.');
        return;
    }

    const rank = (u: vscode.Uri) => (u.fsPath.endsWith('.fsproj') ? 1 : 0);
    const targets = found.sort((a, b) => rank(a) - rank(b) || a.fsPath.localeCompare(b.fsPath));
    let target = targets[0];
    if (targets.length > 1) {
        const pick = await vscode.window.showQuickPick(
            targets.map(u => ({ label: vscode.workspace.asRelativePath(u), uri: u })),
            { placeHolder: 'Solution or project to run fsharp-refactor on' }
        );
        if (!pick) {
            return;
        }
        target = pick.uri;
    }

    const mode = await vscode.window.showQuickPick(
        [
            { label: 'Report only', description: '--dry-run: list the fixes, change nothing', args: '--dry-run' },
            { label: 'Apply fixes', description: 'rewrite the files; every pass is build-verified and rolled back on error', args: '' },
            { label: 'Apply fixes with --api-changes', description: 'also public signatures, names and cross-file rewrites', args: '--api-changes' },
        ],
        { placeHolder: 'How to run fsharp-refactor' }
    );
    if (!mode) {
        return;
    }

    if (!(await toolVersion())) {
        const pick = await vscode.window.showWarningMessage(
            'FSharp.Refactor: the fsharp-refactor dotnet tool is not installed.',
            'Install it'
        );
        if (pick === 'Install it') {
            const t = toolTerminal();
            t.show();
            t.sendText('dotnet tool install -g fsharp-refactor');
        }
        return;
    }

    const t = toolTerminal();
    t.show();
    t.sendText(`${TOOL} "${target.fsPath}" ${mode.args}`.trim());
}

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    // the older copy may already hold these command ids: registration is
    // best effort, and never stops the wiring below
    for (const [id, handler] of [
        ['fsharpRefactor.enable', () => wire(context, true)],
        ['fsharpRefactor.disable', () => unwire(context)],
        ['fsharpRefactor.status', () => status(context)],
        ['fsharpRefactor.run', () => run()],
    ] as const) {
        try {
            context.subscriptions.push(vscode.commands.registerCommand(id, handler));
        } catch {
            // already registered by the older copy; its handler serves both
        }
    }

    void retirePrevious();

    const config = vscode.workspace.getConfiguration(SECTION);
    const current = config.get<string[]>('analyzersPath') ?? [];
    const dir = analyzersDir(context);
    const staleEntry = current.some(p => p.includes(PATH_MARKER) && p !== dir);

    if (current.includes(dir) && (config.get<boolean>('enableAnalyzers') ?? false) && !staleEntry) {
        return; // already wired to THIS version
    }

    if (staleEntry) {
        // an update: the old path is dead, re-point without asking again
        await wire(context, false);
        return;
    }

    if (context.globalState.get<boolean>(DECLINED_KEY)) {
        return; // the user said no; the command remains available
    }

    // settings are the user's — ask before touching them
    const pick = await vscode.window.showInformationMessage(
        'FSharp.Refactor: wire its refactoring analyzers into Ionide? (updates the global FSharp.analyzersPath setting)',
        'Wire it up',
        'Not now'
    );

    if (pick === 'Wire it up') {
        await wire(context, false);
    } else if (pick === 'Not now') {
        await context.globalState.update(DECLINED_KEY, true);
    }
}

export function deactivate(): void {
    // intentionally empty: VS Code offers no reliable uninstall hook, so
    // the settings entry is cleaned up by the update path or the
    // fsharpRefactor.disable command
}
