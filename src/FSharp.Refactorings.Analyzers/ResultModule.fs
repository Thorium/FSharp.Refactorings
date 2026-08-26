/// Refactoring: rewrite manual Ok/Error matching with Result-module functions.
///
///     match r with | Ok v -> Ok (f v)  | Error e -> Error e     →  r |> Result.map (fun v -> f v)
///     match r with | Ok v -> f v       | Error e -> Error e     →  r |> Result.bind (fun v -> f v)
///     match r with | Ok v -> Ok v      | Error e -> Error (g e) →  r |> Result.mapError (fun e -> g e)
///     match r with | Ok v -> Ok v      | Error e -> Error e     →  r
///     match r with | Ok _ -> true      | Error _ -> false       →  r |> Result.isOk
///     match r with | Ok _ -> false     | Error _ -> true        →  r |> Result.isError
///     match r with | Ok v -> v         | Error _ -> d           →  r |> Result.defaultValue d
///     match r with | Ok v -> f v       | Error _ -> ()          →  r |> Result.iter (fun v -> f v)
///     match r with | Ok v -> g v       | Error e -> d           →  r |> Result.map (fun v -> g v) |> Result.defaultWith (fun e -> d)
///
/// `Result.defaultWith` receives the error, so defaults that mention the
/// error's bound variable are supported. Non-atomic defaults always use
/// `defaultWith` to preserve the original laziness. Requires the Result
/// module functions from FSharp.Core 7+.
///
/// Clause order may be reversed. Safety rules mirror the Option analyzer:
/// two guard-free clauses, single-line parts, Ok/Error must resolve to
/// FSharp.Core's Result cases, and the file must have no type errors.
module FSharp.Refactorings.ResultModule

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

type Suggestion =
    {
        /// Range of the whole match expression, i.e. the text the fix replaces.
        Range: range
        OriginalText: string
        ReplacementText: string
        /// The module function used, e.g. "Result.map", or "" for identity.
        Target: string
    }

/// `Ok v` / `Error e` as a pattern: the case ident and the bound variable.
let private casePat (caseName: string) (p: SynPat) =
    match p with
    | SynPat.LongIdent(longDotId = SynLongIdent(id = [ caseIdent ]); argPats = SynArgPats.Pats [ arg ]) when
        caseIdent.idText = caseName
        ->
        boundVar arg |> Option.map (fun v -> caseIdent, v)
    | _ -> None

/// `Ok <e>` / `Error <e>` as an expression.
let private caseApp (caseName: string) (e: SynExpr) =
    match e with
    | SynExpr.App(funcExpr = SynExpr.Ident caseIdent; argExpr = arg) when caseIdent.idText = caseName -> Some arg
    | _ -> None

/// The error clause rewraps its own bound variable: `Error e -> Error e`.
let private rewrapsError (errorVar: string option) (errorBody: SynExpr) =
    match caseApp "Error" errorBody, errorVar with
    | Some(IdentName e), Some v -> e = v
    | _ -> false

/// `defaultValue d` for pure atoms with an unused error, else
/// `defaultWith (fun e -> d)` (the thunk receives the error).
let private defaultCall (source: ISourceText) (errorVar: string option) (defaultBody: SynExpr) =
    let mentionsError =
        match errorVar with
        | Some v ->
            System.Text.RegularExpressions.Regex.IsMatch(
                textOfRange source defaultBody.Range,
                @"\b" + System.Text.RegularExpressions.Regex.Escape v + @"\b"
            )
        | None -> false

    if isPureAtom defaultBody && not mentionsError then
        sprintf "Result.defaultValue %s" (atomicText source defaultBody), "Result.defaultValue"
    else
        let param = if mentionsError then lambdaParam errorVar else "_"

        sprintf "Result.defaultWith (fun %s -> %s)" param (textOfRange source (stripParens defaultBody).Range),
        "Result.defaultWith"

/// Decide the rewrite for an Ok/Error match, given the normalized parts.
let private rewrite
    (source: ISourceText)
    (scrutinee: SynExpr)
    (okVar: string option)
    (okBody: SynExpr)
    (errorVar: string option)
    (errorBody: SynExpr)
    : (string * string) option =
    let pipeSource = atomicText source scrutinee

    let (|OkApp|_|) = caseApp "Ok"
    let (|ErrorApp|_|) = caseApp "Error"

    match okBody, errorBody with
    // ... | Error e -> Error e
    | OkApp(IdentName v), _ when Some v = okVar && rewrapsError errorVar errorBody ->
        // Ok v -> Ok v | Error e -> Error e: the match is the scrutinee itself
        Some(textOfRange source scrutinee.Range, "")
    | OkApp inner, _ when rewrapsError errorVar errorBody ->
        let body = textOfRange source (stripParens inner).Range
        Some(sprintf "%s |> Result.map (fun %s -> %s)" pipeSource (lambdaParam okVar) body, "Result.map")
    | body, _ when rewrapsError errorVar errorBody ->
        let bodyText = textOfRange source (stripParens body).Range
        Some(sprintf "%s |> Result.bind (fun %s -> %s)" pipeSource (lambdaParam okVar) bodyText, "Result.bind")
    // Ok v -> Ok v | Error e -> Error (g e)
    | OkApp(IdentName v), ErrorApp errorInner when Some v = okVar ->
        let body = textOfRange source (stripParens errorInner).Range
        Some(sprintf "%s |> Result.mapError (fun %s -> %s)" pipeSource (lambdaParam errorVar) body, "Result.mapError")
    | BoolConst true, BoolConst false -> Some($"%s{pipeSource} |> Result.isOk", "Result.isOk")
    | BoolConst false, BoolConst true -> Some($"%s{pipeSource} |> Result.isError", "Result.isError")
    | IdentName v, defaultBody when Some v = okVar ->
        let call, target = defaultCall source errorVar defaultBody
        Some($"%s{pipeSource} |> %s{call}", target)
    | body, UnitConst ->
        let bodyText = textOfRange source (stripParens body).Range
        Some(sprintf "%s |> Result.iter (fun %s -> %s)" pipeSource (lambdaParam okVar) bodyText, "Result.iter")
    // the map+default combo, but not when a branch itself constructs a case:
    // the rewrite would still typecheck, yet the original match reads better
    | (OkApp _ | ErrorApp _), _
    | _, (OkApp _ | ErrorApp _) -> None
    | body, defaultBody ->
        let bodyText = textOfRange source (stripParens body).Range
        let call, target = defaultCall source errorVar defaultBody

        Some(
            sprintf "%s |> Result.map (fun %s -> %s) |> %s" pipeSource (lambdaParam okVar) bodyText call,
            $"Result.map + {target}"
        )

/// A candidate found syntactically; the case idents still need resolving
/// against the typed results before the suggestion is emitted.
type private Candidate =
    { MatchRange: range
      OkIdent: Ident
      ErrorIdent: Ident
      Replacement: string
      Target: string }

let private findCandidates (parseTree: ParsedInput) (source: ISourceText) : Candidate list =
    let candidates = ResizeArray<Candidate>()

    let (|OkPat|_|) = casePat "Ok"
    let (|ErrorPat|_|) = casePat "Error"

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(path, expr) =
                match expr with
                | SynExpr.Match(expr = scrutinee; clauses = clauses; range = m) ->
                    // see OptionModule: replacements must be parenthesized when
                    // the match sat unparenthesized as an infix operand
                    let inOperandPosition =
                        match path with
                        | SyntaxNode.SynExpr(SynExpr.App(argExpr = arg)) :: _ -> arg.Range = m
                        | _ -> false

                    let normalized =
                        match clauses |> List.map simpleClause with
                        | [ Some(OkPat(okIdent, okVar), okBody); Some(ErrorPat(errorIdent, errorVar), errorBody) ]
                        | [ Some(ErrorPat(errorIdent, errorVar), errorBody); Some(OkPat(okIdent, okVar), okBody) ] ->
                            Some(okIdent, okVar, okBody, errorIdent, errorVar, errorBody)
                        | _ -> None

                    match normalized with
                    | Some(okIdent, okVar, okBody, errorIdent, errorVar, errorBody) when
                        isSingleLine scrutinee.Range
                        && isSingleLine okBody.Range
                        && isSingleLine errorBody.Range
                        && isPlainBody okBody
                        && isPlainBody errorBody
                        ->
                        match rewrite source scrutinee okVar okBody errorVar errorBody with
                        | Some(replacement, target) ->
                            let replacement =
                                if
                                    inOperandPosition
                                    && not (System.Text.RegularExpressions.Regex.IsMatch(replacement, @"^[\w.]+$"))
                                then
                                    $"({replacement})"
                                else
                                    replacement

                            candidates.Add
                                { MatchRange = m
                                  OkIdent = okIdent
                                  ErrorIdent = errorIdent
                                  Replacement = replacement
                                  Target = target }
                        | None -> ()
                    | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq candidates

/// Find Ok/Error matches that can be rewritten with Result-module functions.
/// Requires typed check results; emits nothing when the file has type errors.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        findCandidates parseTree source
        |> List.filter (fun c ->
            not (spansDirective source c.MatchRange)
            && OptionModule.resolvesToCoreCase check source "Microsoft.FSharp.Core.Result<" c.OkIdent
            && OptionModule.resolvesToCoreCase check source "Microsoft.FSharp.Core.Result<" c.ErrorIdent)
        |> List.map (fun c ->
            { Range = c.MatchRange
              OriginalText = textOfRange source c.MatchRange
              ReplacementText = c.Replacement
              Target = c.Target })
