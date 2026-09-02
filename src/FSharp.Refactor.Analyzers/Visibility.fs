/// Shared scope gate for rules whose fix changes a declaration's COMPILED
/// SHAPE rather than just the code inside it — `[<Struct>]` on a type,
/// `[<return: Struct>]` on an active pattern, field names on a union case.
///
/// Such a rewrite is invisible to F# pattern matching but not to everything
/// else: explicit invocations, first-class uses, reflection, and serializers
/// all see the change. Inside the assembly the compiler (and the apply
/// tool's verification build) catches any breakage; outside it, nothing
/// does. So these rules only fire on declarations that cannot be seen
/// outside this assembly — unless the caller explicitly opted into API
/// changes with `fsharp-refactor --api-changes`.
module FSharp.Refactor.Visibility

open System
open FSharp.Compiler.Syntax

/// True when the apply tool was started with --api-changes, which opts into
/// rewrites that change the assembly's public surface. Never set in editors,
/// so the editor channel always sees the narrow, always-safe rules.
let apiChangesAllowed () =
    Environment.GetEnvironmentVariable "FSREF_API_CHANGES" = "1"

/// Does this modifier hide the declaration from outside the assembly?
let private isNonPublic (accessibility: SynAccess option) =
    match accessibility with
    | Some(SynAccess.Private _ | SynAccess.Internal _) -> true
    | _ -> false

/// Is the declaration invisible outside this assembly — declared
/// private/internal itself, or nested in a private/internal module or
/// namespace? `accessibilities` carries every modifier that hides the part
/// being rewritten; a rule passes the type's modifier, the union
/// representation's, or both, depending on what its edit changes.
let isConfined (path: SyntaxNode list) (accessibilities: SynAccess option list) =
    accessibilities |> List.exists isNonPublic
    || path
       |> List.exists (fun node ->
           match node with
           | SyntaxNode.SynModule(SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo(accessibility = acc))) ->
               isNonPublic acc
           | SyntaxNode.SynModuleOrNamespace(SynModuleOrNamespace(accessibility = acc)) -> isNonPublic acc
           | _ -> false)

/// Is the declaration private — by its own modifier, or a private module
/// around it? Private is the one visibility a signature file never mentions.
let private isPrivate (path: SyntaxNode list) (accessibilities: SynAccess option list) =
    let isPrivateModifier (accessibility: SynAccess option) =
        match accessibility with
        | Some(SynAccess.Private _) -> true
        | _ -> false

    accessibilities |> List.exists isPrivateModifier
    || path
       |> List.exists (fun node ->
           match node with
           | SyntaxNode.SynModule(SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo(accessibility = acc))) ->
               isPrivateModifier acc
           | SyntaxNode.SynModuleOrNamespace(SynModuleOrNamespace(accessibility = acc)) -> isPrivateModifier acc
           | _ -> false)

/// Does a companion .fsi govern the file this declaration sits in?
///
/// A signature must agree with the implementation on the compiled shape —
/// `[<Struct>]` on the type, an active pattern's return type, a union case's
/// field names — for everything it declares, and it declares every internal
/// declaration as well as every public one. Only private escapes it.
/// FR0022, FR0069, FR0093 and FR0130 each found this separately on
/// fcs-fable, which carries 176 signature files; the gate they share now
/// asks once, so FR0011, FR0016 and FR0134 need not find it a fifth time.
let private signatureBound (path: SyntaxNode list) =
    path
    |> List.tryPick (fun node ->
        match node with
        | SyntaxNode.SynModuleOrNamespace(SynModuleOrNamespace(range = r)) -> Some r.FileName
        | _ -> None)
    |> Option.exists Text.hasSignatureFile

/// The gate itself: fire on contained declarations always, on any
/// declaration when the caller opted into API changes — except beside a
/// signature file, where only a private declaration can change shape
/// without the .fsi disagreeing.
let isInScope (allowApiChanges: bool) (path: SyntaxNode list) (accessibilities: SynAccess option list) =
    if signatureBound path then
        isPrivate path accessibilities
    else
        allowApiChanges || isConfined path accessibilities

/// Which DEFINITIONS a scan may touch, for the rules that rewrite a
/// function's signature and every one of its call sites.
///
/// Private is the always-safe editor rule: a private module-level binding
/// has all its call sites in the one file, so the file's own typed results
/// enumerate them. Assembly is the wider project-wide variant, driven only
/// by the apply tool's --api-changes, where call sites are found through
/// the checked project's symbol uses — which is exactly why it stops at
/// effectively-internal declarations. A PUBLIC function's callers can sit
/// in a sibling project of the same repository, or in another repository
/// entirely; the scan cannot see them, so "every use covered" would pass
/// vacuously and the edit would break them (found the hard way: currying
/// SQLProvider.Common's public QueryFactory.createRelated broke
/// SQLProvider.Runtime).
[<RequireQualifiedAccess>]
type Scope =
    | Private
    | Assembly

/// Does a definition with this accessibility, at this path, belong to the
/// given scan?
let scopeMatches (scope: Scope) (path: SyntaxNode list) (accessibility: SynAccess option) =
    match scope with
    | Scope.Private ->
        match accessibility with
        | Some(SynAccess.Private _) -> true
        | _ -> false
    | Scope.Assembly ->
        // a directly-private binding is the single-file rule's territory;
        // this scan takes the rest of the assembly-confined ones
        (match accessibility with
         | Some(SynAccess.Private _) -> false
         | _ -> true)
        && isConfined path [ accessibility ]
