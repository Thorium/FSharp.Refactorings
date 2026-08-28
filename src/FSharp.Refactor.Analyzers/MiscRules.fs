/// Three smaller notes from the CA triage:
///
/// 1. Visible mutable module state (FR0062, CA2211): a non-private
///    module-level `let mutable` is a global variable every consumer can
///    write, with no thread safety and no change tracking.
///
/// 2. Culture-sensitive parsing (FR0067, CA1305): `DateTime.Parse s` and
///    `Double.Parse s` read differently under different server cultures
///    ("1,5" vs "1.5", day/month order); pass CultureInfo.InvariantCulture
///    (or the intended culture) explicitly.
///
/// 3. Duplicate enum values (FR0068, CA1069): two enum cases with the
///    same literal value are usually a copy-paste slip — comparisons and
///    ToString silently conflate them.
module FSharp.Refactor.MiscRules

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type MutableStateSuggestion = { Range: range; Name: string }

type CultureParseSuggestion =
    {
        Range: range
        /// e.g. "DateTime.Parse".
        CallName: string
    }

type DuplicateEnumSuggestion =
    {
        Range: range
        CaseName: string
        /// The earlier case with the same value.
        OriginalName: string
    }

let private cultureSensitiveOwners =
    set [ "DateTime"; "DateTimeOffset"; "Double"; "Single"; "Decimal" ]

/// Find all three. Parse-only.
let find
    (parseTree: ParsedInput)
    (source: ISourceText)
    : MutableStateSuggestion list * CultureParseSuggestion list * DuplicateEnumSuggestion list =
    ignore source
    let index = AstIndex.ofTree parseTree
    let mutables = ResizeArray<MutableStateSuggestion>()
    let parses = ResizeArray<CultureParseSuggestion>()
    let enums = ResizeArray<DuplicateEnumSuggestion>()

    // FR0062: non-private module-level mutables outside private/internal
    // modules
    for path, decl in index.Decls do
        let moduleConfined =
            path
            |> List.exists (fun node ->
                match node with
                | SyntaxNode.SynModule(SynModuleDecl.NestedModule(
                    moduleInfo = SynComponentInfo(accessibility = Some(SynAccess.Private _ | SynAccess.Internal _)))) ->
                    true
                | SyntaxNode.SynModuleOrNamespace(SynModuleOrNamespace(
                    accessibility = Some(SynAccess.Private _ | SynAccess.Internal _))) -> true
                | _ -> false)

        match decl with
        | SynModuleDecl.Let(bindings = bindings) when not moduleConfined ->
            for binding in bindings do
                // the accessibility of `let mutable private x` parses onto
                // the Named pattern, not the binding
                match binding with
                | SynBinding(
                    isMutable = true
                    accessibility = None
                    headPat = SynPat.Named(ident = SynIdent(ident = var); accessibility = None)) ->
                    mutables.Add
                        { Range = var.idRange
                          Name = var.idText }
                | _ -> ()
        | _ -> ()

    for _, e in index.Exprs do
        match e with
        // FR0067: single-argument Parse on culture-sensitive types
        | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = arg) when
            ids.Length >= 2
            && (List.last ids).idText = "Parse"
            && cultureSensitiveOwners.Contains ids.[ids.Length - 2].idText
            ->
            match stripParens arg with
            | SynExpr.Tuple _ -> () // culture already supplied
            | _ ->
                parses.Add
                    { Range = e.Range
                      CallName = ids.[ids.Length - 2].idText + ".Parse" }
        | _ -> ()

    // FR0068: duplicate literal enum values
    for _, decl in index.Decls do
        match decl with
        | SynModuleDecl.Types(typeDefns = defns) ->
            for SynTypeDefn(typeRepr = repr) in defns do
                match repr with
                | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.Enum(cases = cases)) ->
                    let seen = System.Collections.Generic.Dictionary<string, string>()

                    for SynEnumCase(ident = SynIdent(ident = caseId); valueExpr = valueExpr) in cases do
                        let key =
                            match valueExpr with
                            | SynExpr.Const(SynConst.Int32 v, _) -> Some(string v)
                            | SynExpr.Const(SynConst.Int64 v, _) -> Some(string v)
                            | SynExpr.Const(SynConst.Byte v, _) -> Some(string v)
                            | _ -> None

                        match key with
                        | Some k ->
                            match seen.TryGetValue k with
                            | true, original ->
                                enums.Add
                                    { Range = caseId.idRange
                                      CaseName = caseId.idText
                                      OriginalName = original }
                            | _ -> seen.[k] <- caseId.idText
                        | None -> ()
                | _ -> ()
        | _ -> ()

    List.ofSeq mutables, List.ofSeq parses, List.ofSeq enums
