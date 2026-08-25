/// Object-programming correctness rules (the classic inspections):
///
/// 1. A type overriding `Equals` without overriding `GetHashCode` breaks
///    every hash-based container (Dictionary, Set, groupBy, distinct).
///    Hint only — a correct hash needs knowledge of the equality semantics.
///
/// 2. Calling an abstract member from a constructor runs the override
///    before the derived class's construction has finished — its state is
///    not initialized yet. Hint only.
///
/// Both rules are syntactic and scoped to the members declared in the same
/// type definition, which keeps them shadowing-proof and single-file.
module FSharp.Refactorings.ObjectRules

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

type EqualsSuggestion =
    {
        /// The type name, for the message.
        TypeName: string
        /// Range of the Equals member's name, where the hint is anchored.
        Range: range
        OriginalText: string
    }

type CtorCallSuggestion =
    {
        /// The abstract member being called during construction.
        MemberName: string
        Range: range
        OriginalText: string
    }

/// The member name bound by a `member`/`override` definition, with its range.
let private memberName (SynBinding(headPat = headPat)) =
    match headPat with
    | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
    | _ -> None

let private isOverride (SynBinding(valData = SynValData(memberFlags = flags))) =
    flags |> Option.exists (fun f -> f.IsOverrideOrExplicitImpl)

/// All member bindings of a type definition (object-model body plus
/// augmentation-style members).
let private memberBindings (SynTypeDefn(typeRepr = repr; members = extraMembers)) =
    let ofMembers members =
        members
        |> List.collect (fun m ->
            match m with
            | SynMemberDefn.Member(memberDefn = binding) -> [ binding ]
            | _ -> [])

    match repr with
    | SynTypeDefnRepr.ObjectModel(members = members) -> ofMembers members @ ofMembers extraMembers
    | _ -> ofMembers extraMembers

/// Abstract slots declared directly in the type.
let private abstractSlotNames (SynTypeDefn(typeRepr = repr)) =
    match repr with
    | SynTypeDefnRepr.ObjectModel(members = members) ->
        members
        |> List.choose (fun m ->
            match m with
            | SynMemberDefn.AbstractSlot(slotSig = SynValSig(ident = SynIdent(ident = ident))) -> Some ident.idText
            | _ -> None)
        |> Set.ofList
    | _ -> Set.empty

/// The implicit constructor's self identifier (`as this`), if any.
let private selfIdentifier (SynTypeDefn(typeRepr = repr)) =
    match repr with
    | SynTypeDefnRepr.ObjectModel(members = members) ->
        members
        |> List.tryPick (fun m ->
            match m with
            | SynMemberDefn.ImplicitCtor(selfIdentifier = Some self) -> Some self.idText
            | _ -> None)
    | _ -> None

/// Constructor-time expressions: instance let/do bindings in the class body.
let private ctorExprs (SynTypeDefn(typeRepr = repr)) =
    match repr with
    | SynTypeDefnRepr.ObjectModel(members = members) ->
        members
        |> List.collect (fun m ->
            match m with
            | SynMemberDefn.LetBindings(bindings = bindings; isStatic = false) ->
                bindings |> List.map (fun (SynBinding(expr = e)) -> e)
            | _ -> [])
    | _ -> []

/// References to `self.<name>` for any of the given names, anywhere in an
/// expression (worklist walk over the shapes constructors contain).
[<TailCall>]
let rec private selfRefsLoop
    (self: string)
    (names: Set<string>)
    (acc: ResizeArray<Ident>)
    (pending: SynExpr list)
    : unit =
    match pending with
    | [] -> ()
    | e :: rest ->
        let next =
            match e with
            | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ head; name ])) when
                head.idText = self && names.Contains name.idText
                ->
                acc.Add name
                rest
            | SynExpr.DotGet(expr = SynExpr.Ident head; longDotId = SynLongIdent(id = [ name ])) when
                head.idText = self && names.Contains name.idText
                ->
                acc.Add name
                rest
            | SynExpr.Paren(expr = inner)
            | SynExpr.Typed(expr = inner)
            | SynExpr.DotGet(expr = inner)
            | SynExpr.Lambda(body = inner) -> inner :: rest
            | SynExpr.App(funcExpr = f; argExpr = a) -> f :: a :: rest
            | SynExpr.Tuple(exprs = es)
            | SynExpr.ArrayOrList(exprs = es) -> es @ rest
            | SynExpr.ArrayOrListComputed(expr = inner) -> inner :: rest
            | SynExpr.Sequential(expr1 = e1; expr2 = e2) -> e1 :: e2 :: rest
            | SynExpr.IfThenElse(ifExpr = c; thenExpr = t; elseExpr = els) -> c :: t :: (Option.toList els) @ rest
            | SynExpr.Match(expr = scr; clauses = clauses) ->
                scr :: (clauses |> List.map (fun (SynMatchClause(resultExpr = r)) -> r)) @ rest
            | SynExpr.LetOrUse lou -> (lou.Bindings |> List.map (fun (SynBinding(expr = b)) -> b)) @ lou.Body :: rest
            | _ -> rest

        selfRefsLoop self names acc next

/// Find Equals-without-GetHashCode and ctor-time abstract calls.
let find (parseTree: ParsedInput) (source: ISourceText) : EqualsSuggestion list * CtorCallSuggestion list =
    let equalsSuggestions = ResizeArray<EqualsSuggestion>()
    let ctorSuggestions = ResizeArray<CtorCallSuggestion>()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkSynModuleDecl(_path, decl) =
                match decl with
                | SynModuleDecl.Types(typeDefns = typeDefns) ->
                    for SynTypeDefn(typeInfo = SynComponentInfo(longId = typeIds)) as typeDefn in typeDefns do
                        let typeName = typeIds |> List.map (fun i -> i.idText) |> String.concat "."

                        // rule 1: Equals override without GetHashCode override
                        let overrides =
                            memberBindings typeDefn |> List.filter isOverride |> List.choose memberName

                        let equalsIdent = overrides |> List.tryFind (fun i -> i.idText = "Equals")

                        let hasGetHashCode = overrides |> List.exists (fun i -> i.idText = "GetHashCode")

                        match equalsIdent with
                        | Some ident when not hasGetHashCode ->
                            equalsSuggestions.Add
                                { TypeName = typeName
                                  Range = ident.idRange
                                  OriginalText = textOfRange source ident.idRange }
                        | _ -> ()

                        // rule 2: abstract members referenced during construction
                        let abstracts = abstractSlotNames typeDefn

                        match selfIdentifier typeDefn with
                        | Some self when not abstracts.IsEmpty ->
                            let refs = ResizeArray<Ident>()
                            selfRefsLoop self abstracts refs (ctorExprs typeDefn)

                            for ident in refs do
                                ctorSuggestions.Add
                                    { MemberName = ident.idText
                                      Range = ident.idRange
                                      OriginalText = textOfRange source ident.idRange }
                        | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq equalsSuggestions, List.ofSeq ctorSuggestions
