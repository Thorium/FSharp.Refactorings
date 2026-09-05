/// Two type-design notes in the ReSharper tradition:
///
/// 1. Disposable field, no IDisposable (FR0032): a type that CREATES a
///    disposable (`let stream = new FileStream(...)`) but does not
///    implement IDisposable leaves the resource with no owner to dispose
///    it. Only `new`-constructed instance fields count — a field assigned
///    from a constructor parameter is injected, and the injector owns it.
///
/// 2. Could-be-static member (FR0033): an instance member whose body
///    touches no instance state — no self identifier, no instance let
///    field, no primary-constructor parameter, no `base` — can be a
///    `static member`. Advice only: call sites would change from
///    `obj.M(...)` to `Type.M(...)`.
module FSharp.Refactor.ObjectDesign

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type DisposableFieldSuggestion =
    {
        TypeName: string
        FieldName: string
        Range: range
        /// The editor's fix, carried by the type's FIRST such field only:
        /// an `interface System.IDisposable` appended to the type whose
        /// Dispose disposes every created field. Deliberately the plain
        /// form — no Dispose(bool), no finalizer, no GC.SuppressFinalize:
        /// a type holding managed disposables needs none of that.
        Fix: (range * string * string) option
    }

type StaticMemberSuggestion = { MemberName: string; Range: range }

/// FR0047 (CA2213): a disposable field the type's Dispose never touches.
type UndisposedFieldSuggestion =
    {
        TypeName: string
        FieldName: string
        Range: range
        /// The editor's fix: `field.Dispose()` as the first statement of
        /// the type's Dispose body.
        Fix: (range * string * string) option
    }

let private isDisposableName (name: string) = name = "System.IDisposable"

/// Is the type (after abbreviations) IDisposable or an implementation?
let private isDisposableType (t: FSharpType) =
    try
        let t = OptionModule.stripAbbreviations t

        t.HasTypeDefinition
        && (t.TypeDefinition.TryFullName |> Option.exists isDisposableName
            || t.TypeDefinition.AllInterfaces
               |> Seq.exists (fun i ->
                   i.HasTypeDefinition
                   && (i.TypeDefinition.TryFullName |> Option.exists isDisposableName)))
    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
        false

/// Is the entity IDisposable or an implementation?
let entityIsDisposable (entity: FSharpEntity) =
    try
        entity.TryFullName |> Option.exists isDisposableName
        || entity.AllInterfaces
           |> Seq.exists (fun i ->
               i.HasTypeDefinition
               && (i.TypeDefinition.TryFullName |> Option.exists isDisposableName))
    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
        false

/// Does the binder at this location resolve to an IDisposable-implementing
/// type? Shared with the use-binding rule.
let resolvesToDisposable (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value -> isDisposableType value.FullType
        | _ -> false
    | None -> false

/// All names bound by a pattern (primary-constructor parameters).
[<TailCall>]
let rec private patNamesLoop (acc: string list) (pending: SynPat list) =
    match pending with
    | [] -> acc
    | p :: rest ->
        let acc, next =
            match p with
            | SynPat.Named(ident = SynIdent(ident = id)) -> id.idText :: acc, rest
            | SynPat.Typed(pat = inner)
            | SynPat.Attrib(pat = inner)
            | SynPat.Paren(inner, _) -> acc, inner :: rest
            | SynPat.Tuple(elementPats = ps) -> acc, ps @ rest
            | SynPat.LongIdent(argPats = SynArgPats.Pats ps) -> acc, ps @ rest
            | _ -> acc, rest

        patNamesLoop acc next

let private patNames (p: SynPat) : string list = patNamesLoop [] [ p ]

/// Find both kinds of suggestion. Requires typed check results for the
/// disposable gate; the static-member analysis is purely syntactic.
let find
    (parseTree: ParsedInput)
    (source: ISourceText)
    (check: FSharpCheckFileResults)
    : DisposableFieldSuggestion list * StaticMemberSuggestion list * UndisposedFieldSuggestion list =
    let index = AstIndex.ofTree parseTree
    let disposables = ResizeArray<DisposableFieldSuggestion>()
    let statics = ResizeArray<StaticMemberSuggestion>()
    let undisposed = ResizeArray<UndisposedFieldSuggestion>()

    // does any expression inside `r` read or assign one of `names`?
    let mentions (names: Set<string>) (r: range) =
        // the walker does not descend into a record copy-and-update's source
        // (`{ state with ... }`), so read it off the Record node itself
        let copySource (e: SynExpr) =
            match e with
            | SynExpr.Record(copyInfo = Some(copyExpr, _))
            | SynExpr.AnonRecd(copyInfo = Some(copyExpr, _)) ->
                match copyExpr with
                | SynExpr.Ident id -> Some id
                | SynExpr.LongIdent(longDotId = SynLongIdent(id = firstId :: _)) -> Some firstId
                | _ -> None
            | _ -> None

        not names.IsEmpty
        && index.Exprs
           |> Array.exists (fun (_, e) ->
               match e with
               | SynExpr.Ident id when names.Contains id.idText -> Range.rangeContainsRange r id.idRange
               | SynExpr.LongIdent(longDotId = SynLongIdent(id = firstId :: _)) when names.Contains firstId.idText ->
                   Range.rangeContainsRange r firstId.idRange
               | SynExpr.LongIdentSet(SynLongIdent(id = firstId :: _), _, _) when names.Contains firstId.idText ->
                   Range.rangeContainsRange r e.Range
               | _ ->
                   match copySource e with
                   | Some id when names.Contains id.idText -> Range.rangeContainsRange r id.idRange
                   | _ -> false)

    for _, decl in index.Decls do
        match decl with
        | SynModuleDecl.Types(typeDefns = defns) ->
            for typeDefn in defns do
                match typeDefn with
                | SynTypeDefn(
                    typeInfo = SynComponentInfo(longId = typeIds)
                    typeRepr = SynTypeDefnRepr.ObjectModel(members = members)) ->
                    let typeName = typeIds |> List.map (fun i -> i.idText) |> String.concat "."

                    let implementsDisposable =
                        members
                        |> List.exists (fun m ->
                            match m with
                            | SynMemberDefn.Interface(interfaceType = SynType.LongIdent(SynLongIdent(id = ids))) ->
                                not ids.IsEmpty && (List.last ids).idText = "IDisposable"
                            | _ -> false)

                    let instanceLetNames =
                        members
                        |> List.collect (fun m ->
                            match m with
                            | SynMemberDefn.LetBindings(isStatic = false; bindings = bindings) ->
                                bindings
                                |> List.choose (fun (SynBinding(headPat = p)) ->
                                    match p with
                                    | SynPat.Named(ident = SynIdent(ident = var)) -> Some var.idText
                                    | SynPat.LongIdent(longDotId = SynLongIdent(id = [ f ])) -> Some f.idText
                                    | _ -> None)
                            | _ -> [])
                        |> Set.ofList

                    let ctorParamNames =
                        members
                        |> List.collect (fun m ->
                            match m with
                            | SynMemberDefn.ImplicitCtor(ctorArgs = args) -> patNames args
                            | _ -> [])
                        |> Set.ofList

                    // new-constructed disposable instance fields
                    let newDisposableFields =
                        members
                        |> List.collect (fun m ->
                            match m with
                            | SynMemberDefn.LetBindings(isStatic = false; bindings = bindings) ->
                                bindings
                                |> List.choose (fun binding ->
                                    match binding with
                                    | SynBinding(
                                        headPat = SynPat.Named(ident = SynIdent(ident = var)); expr = SynExpr.New _) when
                                        resolvesToDisposable check source var
                                        ->
                                        Some(var.idText, binding.RangeOfBindingWithRhs)
                                    | _ -> None)
                            | _ -> [])

                    // the bodies of Dispose members inside `interface ... with`
                    let disposeBodies =
                        members
                        |> List.collect (fun m ->
                            match m with
                            | SynMemberDefn.Interface(members = Some interfaceMembers) ->
                                interfaceMembers
                                |> List.choose (fun im ->
                                    match im with
                                    | SynMemberDefn.Member(
                                        memberDefn = SynBinding(
                                            headPat = SynPat.LongIdent(longDotId = SynLongIdent(id = ids))
                                            expr = body)) when
                                        not ids.IsEmpty
                                        && (let name = (List.last ids).idText in
                                            name = "Dispose" || name = "DisposeAsync")
                                        ->
                                        // a Dispose that delegates to DisposeAsync
                                        // (FsAutoComplete's progress reporter)
                                        // disposes through the async body
                                        Some body.Range
                                    | _ -> None)
                            | _ -> [])

                    // a member that calls `field.Dispose()` itself — a
                    // Close/Unsubscribe/ref-count protocol — is manual
                    // management, not an ownerless resource
                    let manuallyDisposed (fieldName: string) =
                        members
                        |> List.exists (fun m ->
                            match m with
                            | SynMemberDefn.Member(memberDefn = SynBinding(expr = body)) ->
                                (textOfRange source body.Range).Contains($"{fieldName}.Dispose")
                            | _ -> false)

                    // the members' indentation and the last member's end:
                    // where an appended interface implementation goes
                    let memberIndent =
                        members
                        |> List.tryFind (fun m ->
                            match m with
                            | SynMemberDefn.ImplicitCtor _ -> false
                            | _ -> true)
                        |> Option.map (fun m -> String.replicate m.Range.StartColumn " ")
                        |> Option.defaultValue "    "

                    let membersEnd = members |> List.tryLast |> Option.map (fun m -> m.Range.End)

                    if not implementsDisposable then
                        // FR0032: owns a disposable but is not disposable
                        let leaked =
                            newDisposableFields
                            |> List.filter (fun (fieldName, _) -> not (manuallyDisposed fieldName))

                        // a base class that is disposable already makes an
                        // added `interface IDisposable` a duplicate; a base
                        // that cannot be resolved is not worth the guess
                        let baseAllowsInterface =
                            members
                            |> List.forall (fun m ->
                                match m with
                                | SynMemberDefn.Inherit(baseType = Some(SynType.LongIdent(SynLongIdent(id = ids))))
                                | SynMemberDefn.ImplicitInherit(inheritType = SynType.LongIdent(SynLongIdent(id = ids))) when
                                    not ids.IsEmpty
                                    ->
                                    let id = List.last ids
                                    let r = id.idRange
                                    let lineText = source.GetLineString(r.EndLine - 1)

                                    (match
                                        check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ id.idText ])
                                     with
                                     | Some symbolUse ->
                                         match symbolUse.Symbol with
                                         | :? FSharpEntity as e -> not (entityIsDisposable e)
                                         | :? FSharpMemberOrFunctionOrValue as v ->
                                             (try
                                                 v.IsConstructor
                                                 && (v.DeclaringEntity
                                                     |> Option.map (entityIsDisposable >> not)
                                                     |> Option.defaultValue false)
                                              with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                                                  false)
                                         | _ -> false
                                     | None -> false)
                                | SynMemberDefn.Inherit _
                                | SynMemberDefn.ImplicitInherit _ -> false
                                | _ -> true)

                        // one fix for the type, carried by its first field:
                        // an IDisposable whose Dispose releases every one
                        let typeFix =
                            match membersEnd with
                            | Some at when not leaked.IsEmpty && baseAllowsInterface ->
                                let disposeLines =
                                    leaked
                                    |> List.map (fun (fieldName, _) -> $"{memberIndent}        {fieldName}.Dispose()")
                                    |> String.concat "\n"

                                let insertion =
                                    $"\n\n{memberIndent}interface System.IDisposable with\n{memberIndent}    member _.Dispose() =\n{disposeLines}"

                                Some(Range.mkRange typeDefn.Range.FileName at at, "", insertion)
                            | _ -> None

                        leaked
                        |> List.iteri (fun i (fieldName, fieldRange) ->
                            disposables.Add
                                { TypeName = typeName
                                  FieldName = fieldName
                                  Range = fieldRange
                                  Fix = if i = 0 then typeFix else None })
                    else
                        // FR0047 (CA2213): disposable, but the field is
                        // never touched by any Dispose body
                        for fieldName, fieldRange in newDisposableFields do
                            let touched =
                                disposeBodies
                                |> List.exists (fun body -> mentions (Set.singleton fieldName) body)

                            if not (disposeBodies.IsEmpty || touched) then
                                // `field.Dispose()` as the first statement of
                                // the Dispose body — replacing a `()` body,
                                // else a line above what is there, at its
                                // column
                                let fix =
                                    disposeBodies
                                    |> List.tryHead
                                    |> Option.map (fun body ->
                                        let bodyText = textOfRange source body

                                        if bodyText.Trim() = "()" then
                                            body, bodyText, $"{fieldName}.Dispose()"
                                        else
                                            let at = Range.mkRange body.FileName body.Start body.Start
                                            let indent = String.replicate body.StartColumn " "
                                            at, "", $"{fieldName}.Dispose()\n{indent}")

                                undisposed.Add
                                    { TypeName = typeName
                                      FieldName = fieldName
                                      Range = fieldRange
                                      Fix = fix }

                    // FR0033: instance members touching no instance state —
                    // except where instance-ness is a contract (CE builders,
                    // framework-dispatched subclass members)
                    for m in (if instanceIsContract members then [] else members) do
                        match m with
                        | SynMemberDefn.Member(
                            memberDefn = SynBinding(
                                valData = SynValData(memberFlags = Some flags)
                                attributes = attrs
                                headPat = SynPat.LongIdent(
                                    longDotId = SynLongIdent(id = [ selfId; nameId ]); argPats = SynArgPats.Pats args)
                                expr = bodyExpr)) when
                            flags.IsInstance
                            && not flags.IsOverrideOrExplicitImpl
                            && not flags.IsDispatchSlot
                            && not args.IsEmpty
                            // an ATTRIBUTED member is framework-contract
                            // territory: [<Fact>] tests, [<Benchmark>]
                            // methods (BenchmarkDotNet REQUIRES instance),
                            // [<GlobalSetup>], controller actions — the
                            // framework dispatches reflectively and
                            // instance-ness is part of its protocol
                            && attrs.IsEmpty
                            ->
                            let memberParamNames = args |> List.collect patNames |> Set.ofList

                            let instanceNames =
                                instanceLetNames + ctorParamNames
                                |> Set.add selfId.idText
                                |> Set.add "base"
                                |> fun names -> names - memberParamNames

                            if not (mentions instanceNames bodyExpr.Range) then
                                statics.Add
                                    { MemberName = nameId.idText
                                      Range = m.Range }
                        | _ -> ()
                | _ -> ()
        | _ -> ()

    List.ofSeq disposables, List.ofSeq statics, List.ofSeq undisposed
