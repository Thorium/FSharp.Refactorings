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
    { TypeName: string
      FieldName: string
      Range: range }

type StaticMemberSuggestion = { MemberName: string; Range: range }

/// FR0047 (CA2213): a disposable field the type's Dispose never touches.
type UndisposedFieldSuggestion =
    { TypeName: string
      FieldName: string
      Range: range }

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
    with _ ->
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
                                            expr = body)) when not ids.IsEmpty && (List.last ids).idText = "Dispose" ->
                                        Some body.Range
                                    | _ -> None)
                            | _ -> [])

                    if not implementsDisposable then
                        // FR0032: owns a disposable but is not disposable
                        for fieldName, fieldRange in newDisposableFields do
                            disposables.Add
                                { TypeName = typeName
                                  FieldName = fieldName
                                  Range = fieldRange }
                    else
                        // FR0047 (CA2213): disposable, but the field is
                        // never touched by any Dispose body
                        for fieldName, fieldRange in newDisposableFields do
                            let touched =
                                disposeBodies
                                |> List.exists (fun body -> mentions (Set.singleton fieldName) body)

                            if not (disposeBodies.IsEmpty || touched) then
                                undisposed.Add
                                    { TypeName = typeName
                                      FieldName = fieldName
                                      Range = fieldRange }

                    // FR0033: instance members touching no instance state —
                    // except where instance-ness is a contract (CE builders,
                    // framework-dispatched subclass members)
                    for m in (if instanceIsContract members then [] else members) do
                        match m with
                        | SynMemberDefn.Member(
                            memberDefn = SynBinding(
                                valData = SynValData(memberFlags = Some flags)
                                headPat = SynPat.LongIdent(
                                    longDotId = SynLongIdent(id = [ selfId; nameId ]); argPats = SynArgPats.Pats args)
                                expr = bodyExpr)) when
                            flags.IsInstance
                            && not flags.IsOverrideOrExplicitImpl
                            && not flags.IsDispatchSlot
                            && not args.IsEmpty
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
