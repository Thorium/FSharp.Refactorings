// VS Code has no analyzer concept of its own: F# analyzers load through
// Ionide -> FsAutoComplete -> FSharp.Analyzers.SDK, from the directories
// the `FSharp.analyzersPath` setting names. This extension bundles the
// FSharp.Refactor analyzer assemblies (BOTH SDK builds — FSAC loads the
// one matching its own SDK version and skips the other) and, with the
// user's consent, appends its own analyzers directory to that setting.
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

export async function activate(context: vscode.ExtensionContext): Promise<void> {
    // the older copy may already hold these command ids: registration is
    // best effort, and never stops the wiring below
    for (const [id, handler] of [
        ['fsharpRefactor.enable', () => wire(context, true)],
        ['fsharpRefactor.disable', () => unwire(context)],
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
