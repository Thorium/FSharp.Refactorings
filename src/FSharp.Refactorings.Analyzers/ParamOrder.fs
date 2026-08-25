/// Refactoring (slide 6, single-file variant): swap a private two-parameter
/// function to data-last order when call sites show the eta-blocking shape.
///
///     let private scale x k = x * k        let private scale k x = x * k
///     xs |> List.map (fun x -> scale x 2)  xs |> List.map (scale 2)
///
/// The lambda `fun x -> f x k` is the tell: the varying value arrives first,
/// so the call cannot be partially applied. Swapping the parameters makes
/// the function pipeline-friendly and collapses those lambdas.
///
/// Safety rules:
///   - the function is `private` at module level with exactly two curried
///     parameters, so the typed check results enumerate every call site
///   - at least one call site is `fun x -> f x k` (otherwise the swap is
///     churn); the captured `k` must be a pure atom, because the rewrite
///     `f k` evaluates it once instead of per call
///   - every other use is a direct application `f a b` where at least one
///     argument is a pure atom (swapping argument evaluation order must be
///     unobservable); anything else — partial application, pipe, use as a
///     value — suppresses the suggestion entirely
///   - the file must have no type errors (call shapes are trusted from the
///     typed uses)
module FSharp.Refactorings.ParamOrder

open System.Collections.Generic
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

type Suggestion =
    {
        /// The function being reordered, for the diagnostic message.
        FunctionName: string
        /// Range of the definition's parameter list, where the hint anchors.
        DefRange: range
        /// Definition edits followed by one edit per call site
        /// ((range, original, replacement)).
        Edits: (range * string * string) list
    }

/// A module-level `let private f p1 p2 = ...` definition.
type private Candidate =
    { Ident: Ident
      Param1: SynPat
      Param2: SynPat }

/// Parameter shapes we can swap verbatim: `a`, `_`, `(a: int)`.
let private isSimpleParam (p: SynPat) =
    match p with
    | SynPat.Named _
    | SynPat.Wild _
    | SynPat.Paren(pat = SynPat.Typed(pat = SynPat.Named _)) -> true
    | _ -> false

let private findCandidates (parseTree: ParsedInput) : Candidate list =
    let candidates = ResizeArray<Candidate>()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkSynModuleDecl(_path, decl) =
                match decl with
                | SynModuleDecl.Let(bindings = bindings) ->
                    for SynBinding(headPat = headPat) in bindings do
                        match headPat with
                        | SynPat.LongIdent(
                            longDotId = SynLongIdent(id = [ ident ])
                            accessibility = Some(SynAccess.Private _)
                            argPats = SynArgPats.Pats [ p1; p2 ]) when
                            isSimpleParam p1
                            && isSimpleParam p2
                            && isSingleLine p1.Range
                            && isSingleLine p2.Range
                            ->
                            candidates.Add
                                { Ident = ident
                                  Param1 = p1
                                  Param2 = p2 }
                        | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq candidates

/// How the function identifier is applied at one position in the file.
type private AppSite =
    /// `f a b` (possibly over-applied further; extra args are untouched).
    | TwoArgs of a1: SynExpr * a2: SynExpr
    /// `f a` with nothing more — a partial application.
    | OneArg

/// Every `f ...` application in the file, keyed by the end position of the
/// function identifier so each typed use resolves with one lookup.
let private collectApplications (parseTree: ParsedInput) =
    let apps = Dictionary<int * int, AppSite>()
    // `fun x -> f x k` sites: lambda range and the captured argument
    let lambdas = Dictionary<int * int, range * SynExpr>()

    let key (ident: Ident) =
        ident.idRange.EndLine, ident.idRange.EndColumn

    let index = AstIndex.ofTree parseTree

    // does any identifier expression inside `r` refer to `name`?
    let mentions (name: string) (r: range) =
        index.Exprs
        |> Array.exists (fun (_, e) ->
            match e with
            | SynExpr.Ident id when id.idText = name -> Range.rangeContainsRange r id.idRange
            | SynExpr.LongIdent(longDotId = SynLongIdent(id = firstId :: _)) when firstId.idText = name ->
                Range.rangeContainsRange r e.Range
            | _ -> false)

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(path, expr) =
                match expr with
                | SynExpr.App(
                    isInfix = false
                    funcExpr = SynExpr.App(isInfix = false; funcExpr = SynExpr.Ident ident; argExpr = a1)
                    argExpr = a2) -> apps.[key ident] <- TwoArgs(a1, a2)
                | SynExpr.App(isInfix = false; funcExpr = SynExpr.Ident ident; argExpr = _) ->
                    // the inner node of an `f a b` chain is visited too;
                    // only a lone `f a` is a partial application
                    let partOfChain =
                        match path with
                        | SyntaxNode.SynExpr(SynExpr.App(funcExpr = parentFunc)) :: _ -> parentFunc.Range = expr.Range
                        | _ -> false

                    if not partOfChain then
                        apps.[key ident] <- OneArg
                | SynExpr.Lambda(parsedData = Some([ SynPat.Named(ident = SynIdent(ident = x)) ], lambdaBody)) when
                    isSingleLine expr.Range
                    ->
                    match stripParens lambdaBody with
                    | SynExpr.App(
                        isInfix = false
                        funcExpr = SynExpr.App(
                            isInfix = false; funcExpr = SynExpr.Ident ident; argExpr = SynExpr.Ident dataArg)
                        argExpr = captured) when
                        dataArg.idText = x.idText
                        && isPureAtom captured
                        && not (mentions x.idText captured.Range)
                        ->
                        lambdas.[key ident] <- (expr.Range, captured)
                    | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    apps, lambdas

/// The swap edits for one direct application `f a b` → `f b a`, or None when
/// the use is not swappable.
let private callEdits (source: ISourceText) (funcEnd: pos) (a1: SynExpr) (a2: SynExpr) =
    if
        isSingleLine a1.Range
        && isSingleLine a2.Range
        && (isPureAtom (stripParens a1) || isPureAtom (stripParens a2))
    then
        let text1 = textOfRange source a1.Range
        let text2 = textOfRange source a2.Range

        // `f(a) b`: the first argument touches the identifier, so the
        // incoming text needs a separating space
        let touching = funcEnd = a1.Range.Start
        let replacement1 = (if touching then " " else "") + text2

        Some [ a1.Range, text1, replacement1; a2.Range, text2, text1 ]
    else
        None

/// Find private data-first two-parameter functions with eta-blocking lambda
/// call sites, and build the definition + call-site edits. Requires typed
/// check results.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    let hasErrors =
        check.Diagnostics
        |> Array.exists (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

    if hasErrors then
        []
    else
        match findCandidates parseTree with
        | [] -> []
        | candidates ->

            // collected only when the file actually has candidates
            let apps, lambdas = collectApplications parseTree

            candidates
            |> List.choose (fun candidate ->
                let r = candidate.Ident.idRange
                let lineText = source.GetLineString(r.EndLine - 1)

                match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ candidate.Ident.idText ]) with
                | None -> None
                | Some symbolUse ->
                    let uses =
                        check.GetUsesOfSymbolInFile symbolUse.Symbol
                        |> Array.filter (fun u -> not u.IsFromDefinition)

                    let lambdaCount =
                        uses
                        |> Array.filter (fun u -> lambdas.ContainsKey(u.Range.EndLine, u.Range.EndColumn))
                        |> Array.length

                    let siteEdits =
                        uses
                        |> Array.map (fun u ->
                            let useKey = u.Range.EndLine, u.Range.EndColumn

                            match lambdas.TryGetValue useKey with
                            | true, (lambdaRange, captured) ->
                                Some
                                    [ lambdaRange,
                                      textOfRange source lambdaRange,
                                      candidate.Ident.idText + " " + argumentText source captured ]
                            | _ ->
                                match apps.TryGetValue useKey with
                                | true, TwoArgs(a1, a2) -> callEdits source u.Range.End a1 a2
                                | _ -> None)

                    if lambdaCount = 0 || siteEdits |> Array.exists Option.isNone then
                        None
                    else
                        let p1Text = textOfRange source candidate.Param1.Range
                        let p2Text = textOfRange source candidate.Param2.Range

                        let touching = candidate.Ident.idRange.End = candidate.Param1.Range.Start

                        let defEdits =
                            [ candidate.Param1.Range, p1Text, (if touching then " " else "") + p2Text
                              candidate.Param2.Range, p2Text, p1Text ]

                        let defRange = Range.unionRanges candidate.Param1.Range candidate.Param2.Range

                        Some
                            { FunctionName = candidate.Ident.idText
                              DefRange = defRange
                              Edits = defEdits @ (siteEdits |> Array.toList |> List.collect Option.get) })
