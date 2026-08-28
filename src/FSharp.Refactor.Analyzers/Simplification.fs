/// Refactoring: simplify common boolean, option-comparison, and emptiness
/// idioms.
///
///     if c then true else false        →  c
///     if c then false else true        →  not c
///     x = None      /  None = x        →  x |> Option.isNone      (typed-gated)
///     x <> None                        →  x |> Option.isSome
///     x = ValueNone                    →  x |> ValueOption.isNone
///     List.length xs = 0               →  List.isEmpty xs
///     xs |> Seq.length = 0             →  xs |> Seq.isEmpty
///     Array.length xs > 0              →  not (Array.isEmpty xs)
///     Set.count s = 0                  →  Set.isEmpty s
///
/// The emptiness rewrite is also a performance fix for Seq: `Seq.length`
/// forces the whole sequence, `Seq.isEmpty` looks at one element.
///
/// The boolean and emptiness rules are parse-only (the collection module
/// name pins the type); the None-comparison rules require typed check
/// results proving the case is really FSharp.Core's None/ValueNone.
module FSharp.Refactor.Simplification

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactor.Text

[<RequireQualifiedAccess>]
type SimplificationKind =
    /// `if c then true else false` / `if c then false else true`
    | BooleanIdentity
    /// `x = None`, `x <> ValueNone`, ...
    | OptionComparison
    /// `List.length xs = 0`, `xs |> Seq.length > 0`, ...
    | Emptiness

type Suggestion =
    { Range: range
      OriginalText: string
      ReplacementText: string
      Kind: SimplificationKind }

/// `lhs OP rhs` for a named infix operator.
[<return: Struct>]
let private (|InfixApp|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = IdentName op; argExpr = lhs); argExpr = rhs) when
        op.StartsWith "op_"
        ->
        ValueSome(op, lhs, rhs)
    | _ -> ValueNone

/// A bare `None` or `ValueNone` expression, with the module and FullName
/// prefix needed for the rewrite and the typed gate.
[<return: Struct>]
let private (|NoneCaseIdent|_|) (e: SynExpr) =
    match e with
    | SynExpr.Ident ident when ident.idText = "None" -> ValueSome(ident, "Option", "Microsoft.FSharp.Core.Option<")
    | SynExpr.Ident ident when ident.idText = "ValueNone" ->
        ValueSome(ident, "ValueOption", "Microsoft.FSharp.Core.ValueOption<")
    | _ -> ValueNone

/// `M.length` / `M.count` for a collection module with an isEmpty function.
[<return: Struct>]
let private (|LengthFunc|_|) (e: SynExpr) =
    match e with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ])) ->
        match m.idText, f.idText with
        | ("List" | "Seq" | "Array"), "length" -> ValueSome m.idText
        | ("Set" | "Map"), "count" -> ValueSome m.idText
        | _ -> ValueNone
    | _ -> ValueNone

/// A length/count expression: direct `M.length xs` or piped `xs |> M.length`.
/// Returns the module name, the collection argument, and whether it was piped.
[<return: Struct>]
let private (|LengthOf|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = LengthFunc m; argExpr = arg) -> ValueSome(m, arg, false)
    | PipeApp(arg, LengthFunc m) -> ValueSome(m, arg, true)
    | _ -> ValueNone

/// Find simplifiable expressions. `check` enables the typed None-comparison
/// rules; without it only the parse-only rules run.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults option) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()

    let add (range: range) (replacement: string) kind =
        suggestions.Add
            { Range = range
              OriginalText = textOfRange source range
              ReplacementText = replacement
              Kind = kind }

    let noneComparison (range: range) (op: string) (other: SynExpr) (ident: Ident) (m: string) (prefix: string) =
        let gate =
            check
            |> Option.exists (fun check -> OptionModule.resolvesToCoreCase check source prefix ident)

        if gate && isSingleLine other.Range then
            let fn = if op = "op_Equality" then "isNone" else "isSome"
            add range (sprintf "%s |> %s.%s" (atomicText source other) m fn) SimplificationKind.OptionComparison

    let emptiness (range: range) (negated: bool) (m: string) (arg: SynExpr) (piped: bool) =
        if isSingleLine arg.Range then
            let replacement =
                match piped, negated with
                | true, false -> sprintf "%s |> %s.isEmpty" (textOfRange source arg.Range) m
                | true, true -> sprintf "%s |> %s.isEmpty |> not" (textOfRange source arg.Range) m
                | false, false -> sprintf "%s.isEmpty %s" m (textOfRange source arg.Range)
                | false, true -> sprintf "not (%s.isEmpty %s)" m (textOfRange source arg.Range)

            add range replacement SimplificationKind.Emptiness

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path, expr) =
                match expr with
                // if c then true else false / if c then false else true
                // trivia.IsElif guard: an elif node's range starts at the elif
                // keyword, so replacing it with the bare condition would glue
                // the condition onto the preceding branch
                | SynExpr.IfThenElse(
                    ifExpr = cond; thenExpr = BoolConst thenValue; elseExpr = Some(BoolConst elseValue); trivia = trivia) when
                    not trivia.IsElif
                    && thenValue <> elseValue
                    && isSingleLine cond.Range
                    && isSafeInline cond
                    ->
                    let replacement =
                        if thenValue then
                            textOfRange source cond.Range
                        else
                            "not " + atomicText source cond

                    add expr.Range replacement SimplificationKind.BooleanIdentity
                // x = None / None = x / x <> None (and ValueNone)
                | InfixApp(("op_Equality" | "op_Inequality") as op, NoneCaseIdent(ident, m, prefix), other)
                | InfixApp(("op_Equality" | "op_Inequality") as op, other, NoneCaseIdent(ident, m, prefix)) ->
                    noneComparison expr.Range op other ident m prefix
                // length/count compared with zero
                | InfixApp("op_Equality", LengthOf(m, arg, piped), ZeroConst)
                | InfixApp("op_Equality", ZeroConst, LengthOf(m, arg, piped)) -> emptiness expr.Range false m arg piped
                | InfixApp("op_Inequality", LengthOf(m, arg, piped), ZeroConst)
                | InfixApp("op_Inequality", ZeroConst, LengthOf(m, arg, piped))
                | InfixApp("op_GreaterThan", LengthOf(m, arg, piped), ZeroConst)
                | InfixApp("op_LessThan", ZeroConst, LengthOf(m, arg, piped)) -> emptiness expr.Range true m arg piped
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq suggestions
