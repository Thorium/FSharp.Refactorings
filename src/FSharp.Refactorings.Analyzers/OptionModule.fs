/// Refactoring: rewrite manual Some/None (and ValueSome/ValueNone) matching
/// with Option-module (ValueOption-module) functions.
///
///     match x with | Some v -> Some (f v) | None -> None   →  x |> Option.map (fun v -> f v)
///     match x with | Some v -> f v        | None -> None   →  x |> Option.bind (fun v -> f v)
///     match x with | Some v -> v          | None -> None   →  x |> Option.flatten
///     match x with | Some v -> Some v     | None -> None   →  x
///     match x with | Some v -> v          | None -> d      →  x |> Option.defaultValue d
///     match x with | Some _ -> true       | None -> false  →  x |> Option.isSome
///     match x with | Some _ -> false      | None -> true   →  x |> Option.isNone
///     match x with | Some v -> f v        | None -> ()     →  x |> Option.iter (fun v -> f v)
///     match x with | Some v -> g v        | None -> d      →  x |> Option.map (fun v -> g v) |> Option.defaultValue d
///
/// The same shapes are recognized for ValueSome/ValueNone, rewritten with the
/// ValueOption module. Default values that are not pure atoms (identifiers or
/// constants) are wrapped as `defaultWith (fun () -> d)` instead, preserving
/// the original laziness: the match evaluated the default only in the None
/// branch, and `defaultValue` would evaluate it always.
///
/// Clause order may be reversed. Safety rules:
///   - exactly two clauses, no `when` guards, single-line scrutinee and bodies
///   - the case names must resolve to FSharp.Core's option/voption cases
///     (checked against the typed results, so shadowing user types never
///     produces a wrong rewrite)
///   - the file must have no type errors (the bind rule relies on the match
///     having typechecked: when the none branch is `None`, the some branch is
///     known to be option-typed)
///   - non-atomic expressions are parenthesized when inlined
module FSharp.Refactorings.OptionModule

open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

/// Names for one wrapper family: Some/None/Option or ValueSome/ValueNone/ValueOption.
type WrapperConfig =
    {
        /// The value-carrying case, e.g. "Some".
        SomeName: string
        /// The empty case, e.g. "None".
        NoneName: string
        /// The module whose functions replace the match, e.g. "Option".
        ModuleName: string
        /// FullName prefix that proves a case belongs to FSharp.Core.
        CoreFullNamePrefix: string
    }

let optionConfig =
    { SomeName = "Some"
      NoneName = "None"
      ModuleName = "Option"
      // anchored with '<' so a hypothetical Option2 type cannot prefix-match
      CoreFullNamePrefix = "Microsoft.FSharp.Core.Option<" }

let valueOptionConfig =
    { SomeName = "ValueSome"
      NoneName = "ValueNone"
      ModuleName = "ValueOption"
      CoreFullNamePrefix = "Microsoft.FSharp.Core.ValueOption<" }

type Suggestion =
    {
        /// Range of the whole match expression, i.e. the text the fix replaces.
        Range: range
        OriginalText: string
        ReplacementText: string
        /// The module function used, e.g. "Option.map", or "" for identity.
        Target: string
    }

/// `Some v`, `Some (v)`, `Some _` as a pattern: returns the case ident (for
/// symbol resolution) and the bound variable name (None for wildcard).
let private somePat (cfg: WrapperConfig) (p: SynPat) =
    match p with
    | SynPat.LongIdent(longDotId = SynLongIdent(id = [ someIdent ]); argPats = SynArgPats.Pats [ arg ]) when
        someIdent.idText = cfg.SomeName
        ->
        boundVar arg |> Option.map (fun v -> someIdent, v)
    | _ -> None

/// `None` as a pattern: returns the case ident for symbol resolution.
let private nonePat (cfg: WrapperConfig) (p: SynPat) =
    match p with
    | SynPat.LongIdent(longDotId = SynLongIdent(id = [ noneIdent ]); argPats = SynArgPats.Pats []) when
        noneIdent.idText = cfg.NoneName
        ->
        Some noneIdent
    | _ -> None

/// `<pipe> |> M.defaultValue d` for pure atoms, else `<pipe> |> M.defaultWith (fun () -> d)`.
let private defaultCall (cfg: WrapperConfig) (source: ISourceText) (defaultBody: SynExpr) =
    if isPureAtom defaultBody then
        sprintf "%s.defaultValue %s" cfg.ModuleName (atomicText source defaultBody), $"{cfg.ModuleName}.defaultValue"
    else
        sprintf "%s.defaultWith (fun () -> %s)" cfg.ModuleName (textOfRange source defaultBody.Range),
        $"{cfg.ModuleName}.defaultWith"

/// A candidate found syntactically; the case idents still need to be resolved
/// against the typed results before the suggestion is emitted.
type private Candidate =
    { MatchRange: range
      SomeIdent: Ident
      NoneIdent: Ident
      Replacement: string
      Target: string }

/// Decide the rewrite for a wrapper match, given the normalized parts.
let private rewrite
    (cfg: WrapperConfig)
    (source: ISourceText)
    (scrutinee: SynExpr)
    (boundVar: string option)
    (someBody: SynExpr)
    (noneBody: SynExpr)
    : (string * string) option =
    let pipeSource = atomicText source scrutinee
    let m = cfg.ModuleName

    /// `Some <e>` as an expression.
    let (|SomeApp|_|) (e: SynExpr) =
        match e with
        | SynExpr.App(funcExpr = SynExpr.Ident someIdent; argExpr = arg) when someIdent.idText = cfg.SomeName ->
            Some arg
        | _ -> None

    let (|NoneIdent|_|) (e: SynExpr) =
        match e with
        | IdentName t when t = cfg.NoneName -> Some()
        | _ -> None

    match someBody, noneBody with
    // ... | None -> None
    | SomeApp(IdentName v), NoneIdent when Some v = boundVar ->
        // Some v -> Some v: the whole match is the scrutinee itself
        Some(textOfRange source scrutinee.Range, "")
    | SomeApp inner, NoneIdent ->
        let body = textOfRange source (stripParens inner).Range
        Some(sprintf "%s |> %s.map (fun %s -> %s)" pipeSource m (lambdaParam boundVar) body, $"{m}.map")
    | IdentName v, NoneIdent when Some v = boundVar -> Some($"%s{pipeSource} |> %s{m}.flatten", $"{m}.flatten")
    | body, NoneIdent ->
        let bodyText = textOfRange source (stripParens body).Range
        Some(sprintf "%s |> %s.bind (fun %s -> %s)" pipeSource m (lambdaParam boundVar) bodyText, $"{m}.bind")
    // ... | None -> <something else>
    | BoolConst true, BoolConst false -> Some($"%s{pipeSource} |> %s{m}.isSome", $"{m}.isSome")
    | BoolConst false, BoolConst true -> Some($"%s{pipeSource} |> %s{m}.isNone", $"{m}.isNone")
    | IdentName v, defaultBody when Some v = boundVar ->
        let call, target = defaultCall cfg source defaultBody
        Some($"%s{pipeSource} |> %s{call}", target)
    | body, UnitConst ->
        let bodyText = textOfRange source (stripParens body).Range
        Some(sprintf "%s |> %s.iter (fun %s -> %s)" pipeSource m (lambdaParam boundVar) bodyText, $"{m}.iter")
    // `Some v -> pred v | None -> false/true` are exists/forall
    | body, BoolConst false ->
        let bodyText = textOfRange source (stripParens body).Range
        Some(sprintf "%s |> %s.exists (fun %s -> %s)" pipeSource m (lambdaParam boundVar) bodyText, $"{m}.exists")
    | body, BoolConst true ->
        let bodyText = textOfRange source (stripParens body).Range
        Some(sprintf "%s |> %s.forall (fun %s -> %s)" pipeSource m (lambdaParam boundVar) bodyText, $"{m}.forall")
    // the map+default combo, but not when a branch itself constructs a case:
    // the rewrite would still typecheck, yet the original match reads better
    | (SomeApp _ | NoneIdent), _
    | _, (SomeApp _ | NoneIdent) -> None
    | body, defaultBody ->
        let bodyText = textOfRange source (stripParens body).Range
        let call, _ = defaultCall cfg source defaultBody

        Some(sprintf "%s |> %s.map (fun %s -> %s) |> %s" pipeSource m (lambdaParam boundVar) bodyText call, $"{m}.map")

let private findCandidates (cfg: WrapperConfig) (parseTree: ParsedInput) (source: ISourceText) : Candidate list =
    let candidates = ResizeArray<Candidate>()

    let (|SomePat|_|) = somePat cfg
    let (|NonePat|_|) = nonePat cfg

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(path, expr) =
                match expr with
                | SynExpr.Match(expr = scrutinee; clauses = clauses; range = m) ->
                    // a match may sit unparenthesized as an infix operand
                    // (`1 + match ...`); our pipeline replacement is not as
                    // greedy as the match was, so it must be parenthesized there
                    let inOperandPosition =
                        match path with
                        | SyntaxNode.SynExpr(SynExpr.App(argExpr = arg)) :: _ -> arg.Range = m
                        | _ -> false

                    let normalized =
                        match clauses |> List.map simpleClause with
                        | [ Some(SomePat(someIdent, boundVar), someBody); Some(NonePat noneIdent, noneBody) ]
                        | [ Some(NonePat noneIdent, noneBody); Some(SomePat(someIdent, boundVar), someBody) ] ->
                            Some(someIdent, boundVar, someBody, noneIdent, noneBody)
                        | _ -> None

                    match normalized with
                    | Some(someIdent, boundVar, someBody, noneIdent, noneBody) when
                        isSingleLine scrutinee.Range
                        && isSingleLine someBody.Range
                        && isSingleLine noneBody.Range
                        && isPlainBody someBody
                        && isPlainBody noneBody
                        ->
                        match rewrite cfg source scrutinee boundVar someBody noneBody with
                        | Some(replacement, target) ->
                            let replacement =
                                if inOperandPosition && not (Regex.IsMatch(replacement, @"^[\w.]+$")) then
                                    "(" + replacement + ")"
                                else
                                    replacement

                            candidates.Add
                                { MatchRange = m
                                  SomeIdent = someIdent
                                  NoneIdent = noneIdent
                                  Replacement = replacement
                                  Target = target }
                        | None -> ()
                    | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq candidates

/// True when the ident at this location resolves to a union case whose
/// FullName starts with the given FSharp.Core prefix. Shared with other
/// analyzers that must prove an ident is really e.g. option's None.
let resolvesToCoreCase (check: FSharpCheckFileResults) (source: ISourceText) (prefix: string) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpUnionCase as unionCase ->
            // e.g. "Microsoft.FSharp.Core.Option<_>.Some"
            (try
                unionCase.FullName
             with _ ->
                 "")
                .StartsWith
                prefix
        | _ -> false
    | None -> false

/// True when the identifier resolves into FSharp.Core's Operators module —
/// guards rules that pattern-match on names like `isNull`, `sprintf`, or
/// `(+)` against user-defined shadowing.
let resolvesToCoreOperator (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value ->
            (try
                value.FullName
             with _ ->
                 "")
                .StartsWith
                "Microsoft.FSharp.Core.Operators"
        | _ -> false
    | None -> false

/// True when the file's typed results contain any error diagnostics.
let hasErrors (check: FSharpCheckFileResults) =
    check.Diagnostics
    |> Array.exists (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

/// Find all wrapper matches for one config (Option or ValueOption) that can
/// be rewritten with module functions. Requires typed check results; emits
/// nothing when the file has type errors.
let findWith (cfg: WrapperConfig) (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) =
    if hasErrors check then
        []
    else
        findCandidates cfg parseTree source
        |> List.filter (fun c ->
            resolvesToCoreCase check source cfg.CoreFullNamePrefix c.SomeIdent
            && resolvesToCoreCase check source cfg.CoreFullNamePrefix c.NoneIdent)
        |> List.map (fun c ->
            { Range = c.MatchRange
              OriginalText = textOfRange source c.MatchRange
              ReplacementText = c.Replacement
              Target = c.Target })

/// Find Option and ValueOption matches that can be rewritten.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    findWith optionConfig parseTree source check
    @ findWith valueOptionConfig parseTree source check
