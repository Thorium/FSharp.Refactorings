/// Refactoring: convert a private function's tupled parameter to curried
/// parameters, updating every call site.
///
///     let private add (a, b) = a + b        →  let private add a b = a + b
///     let total = add (1, 2)                →  let total = add 1 2
///     let plus = add (x + 1, g y)           →  let plus = add (x + 1) (g y)
///
/// Curried parameters are the idiomatic form for F#-internal code (tupled is
/// for interop), so per the project policy only that direction is offered.
///
/// Safety rules:
///   - the function must be `private` at module level, so every call site is
///     in this file and the typed check results enumerate all of them
///   - every use must be a direct application `f (x, y)` with matching tuple
///     arity — a use as a first-class value, a pipe `(a, b) |> f`, or a
///     partial application suppresses the suggestion entirely
///   - tuple elements must be simple names, `_`, or annotated names; the
///     definition pattern and every call tuple must be single-line
///   - the file must have no type errors (call shapes are trusted from the
///     typed uses)
module FSharp.Refactorings.TupleParams

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

/// A single text edit: range, original text, replacement text.
type Edit =
    { Range: range
      Original: string
      Replacement: string }

type Suggestion =
    {
        /// The function being curried, for the diagnostic message.
        FunctionName: string
        /// Range of the whole definition pattern, where the hint is anchored.
        DefRange: range
        /// The definition edit followed by one edit per call site.
        Edits: Edit list
    }

/// Expressions that can appear bare as a curried argument; anything else is
/// parenthesized, as is anything starting with `-` (it would parse as
/// subtraction).
let private isAtomicArg (e: SynExpr) =
    match e with
    | SynExpr.Ident _
    | SynExpr.LongIdent _
    | SynExpr.Const _
    | SynExpr.Paren _
    | SynExpr.DotGet _ -> true
    | SynExpr.App(flag = ExprAtomicFlag.Atomic) -> true
    | _ -> false

let private argText (source: ISourceText) (e: SynExpr) =
    let text = textOfRange source e.Range

    if isAtomicArg e && not (text.StartsWith '-') then
        text
    else
        $"({text})"

/// A tuple element that can become a curried parameter: `a`, `_`, `a: int`.
let private renderParam (source: ISourceText) (p: SynPat) =
    match p with
    | SynPat.Named _
    | SynPat.Wild _ -> Some(textOfRange source p.Range)
    | SynPat.Typed(pat = SynPat.Named _) -> Some("(" + textOfRange source p.Range + ")")
    | _ -> None

/// A module-level `let private f (a, b) = ...` definition.
type private Candidate =
    { Ident: Ident
      ParenPatRange: range
      Elements: SynPat list }

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
                            argPats = SynArgPats.Pats [ SynPat.Paren(pat = SynPat.Tuple(elementPats = elements)) as paren ]) when
                            elements.Length >= 2 && isSingleLine paren.Range
                            ->
                            candidates.Add
                                { Ident = ident
                                  ParenPatRange = paren.Range
                                  Elements = elements }
                        | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq candidates

/// Every application node `f args` in the file, for matching against symbol
/// uses (the function expression's end position coincides with the use's).
/// An application directly under a projection (`f(1, 2).Length`, `f(1, 2).[0]`)
/// is marked unsafe: currying it would lose the atomic grouping.
let private collectApplications (parseTree: ParsedInput) =
    let apps =
        System.Collections.Generic.Dictionary<int * int, SynExpr * SynExpr * bool>()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(path, expr) =
                match expr with
                | SynExpr.App(isInfix = false; funcExpr = funcExpr; argExpr = argExpr) ->
                    match funcExpr with
                    | SynExpr.Ident _
                    | SynExpr.LongIdent _ ->
                        let projected =
                            match path with
                            | SyntaxNode.SynExpr(SynExpr.DotGet _) :: _
                            | SyntaxNode.SynExpr(SynExpr.DotIndexedGet _) :: _ -> true
                            | _ -> false

                        apps.[(funcExpr.Range.EndLine, funcExpr.Range.EndColumn)] <- (funcExpr, argExpr, projected)
                    | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    apps

/// Build the call-site edit for one use, or None when the use is not a direct
/// full application of the tuple. Applications are indexed by the function
/// expression's end position, so each use is a dictionary lookup rather than
/// a scan over every application in the file.
let private callEdit
    (source: ISourceText)
    (apps: System.Collections.Generic.Dictionary<int * int, SynExpr * SynExpr * bool>)
    (arity: int)
    (useRange: range)
    : Edit option =
    match apps.TryGetValue((useRange.EndLine, useRange.EndColumn)) with
    | true, (funcExpr, argExpr, projected) ->
        match argExpr with
        | SynExpr.Paren(expr = SynExpr.Tuple(exprs = args)) when
            args.Length = arity && isSingleLine argExpr.Range && not projected
            ->
            let curried = args |> List.map (argText source) |> String.concat " "

            let replacement =
                // `f(1, 2)` has no space between the name and the tuple
                if funcExpr.Range.End = argExpr.Range.Start then
                    $" {curried}"
                else
                    curried

            Some
                { Range = argExpr.Range
                  Original = textOfRange source argExpr.Range
                  Replacement = replacement }
        | _ -> None
    | false, _ -> None

/// Find private tupled functions whose every use is a direct call, and build
/// the definition + call-site edits. Requires typed check results.
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
            let apps = collectApplications parseTree

            candidates
            |> List.choose (fun candidate ->
                let paramTexts = candidate.Elements |> List.map (renderParam source)

                if paramTexts |> List.exists Option.isNone then
                    None
                else
                    let r = candidate.Ident.idRange
                    let lineText = source.GetLineString(r.EndLine - 1)

                    match
                        check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ candidate.Ident.idText ])
                    with
                    | None -> None
                    | Some symbolUse ->
                        let uses =
                            check.GetUsesOfSymbolInFile symbolUse.Symbol
                            |> Array.filter (fun u -> not u.IsFromDefinition)

                        let callEdits =
                            uses
                            |> Array.map (fun u -> callEdit source apps candidate.Elements.Length u.Range)

                        if callEdits |> Array.exists Option.isNone then
                            None
                        else
                            let curriedParams = paramTexts |> List.map Option.get |> String.concat " "

                            let defEdit =
                                { Range = candidate.ParenPatRange
                                  Original = textOfRange source candidate.ParenPatRange
                                  Replacement =
                                    // `let private add(a, b)` has no space before the tuple
                                    if candidate.Ident.idRange.End = candidate.ParenPatRange.Start then
                                        $" {curriedParams}"
                                    else
                                        curriedParams }

                            Some
                                { FunctionName = candidate.Ident.idText
                                  DefRange = candidate.ParenPatRange
                                  Edits = defEdit :: (callEdits |> Array.map Option.get |> Array.toList) })
