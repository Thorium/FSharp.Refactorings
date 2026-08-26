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
module FSharp.Refactorings.Visibility

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

/// The gate itself: fire on contained declarations always, on any
/// declaration when the caller opted into API changes.
let isInScope (allowApiChanges: bool) (path: SyntaxNode list) (accessibilities: SynAccess option list) =
    allowApiChanges || isConfined path accessibilities

/// Which DEFINITIONS a scan may touch, for the rules that rewrite a
/// function's signature and every one of its call sites.
///
/// Private is the always-safe editor rule: a private module-level binding
/// has all its call sites in the one file, so the file's own typed results
/// enumerate them. NonPrivate is the API-CHANGING project-wide variant,
/// driven only by the apply tool's --api-changes, where the call sites are
/// found through the whole project's symbol uses.
[<RequireQualifiedAccess>]
type Scope =
    | Private
    | NonPrivate

/// Does a definition with this accessibility belong to the given scan?
let scopeMatches (scope: Scope) (accessibility: SynAccess option) =
    match scope, accessibility with
    | Scope.Private, Some(SynAccess.Private _) -> true
    | Scope.NonPrivate, (None | Some(SynAccess.Internal _) | Some(SynAccess.Public _)) -> true
    | _ -> false
