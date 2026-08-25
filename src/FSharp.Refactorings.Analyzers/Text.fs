/// Helpers for turning FCS ranges back into source text.
/// All refactorings emit minimal range-based edits, so extracting the exact
/// original text of a sub-expression is the core primitive.
module FSharp.Refactorings.Text

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

/// The exact source text covered by a range.
let textOfRange (source: ISourceText) (r: range) : string =
    if r.StartLine = r.EndLine then
        source.GetLineString(r.StartLine - 1).Substring(r.StartColumn, r.EndColumn - r.StartColumn)
    else
        [ for lineNumber in r.StartLine .. r.EndLine do
              let line = source.GetLineString(lineNumber - 1)

              if lineNumber = r.StartLine then
                  line.Substring(r.StartColumn)
              elif lineNumber = r.EndLine then
                  line.Substring(0, r.EndColumn)
              else
                  line ]
        |> String.concat "\n"

let isSingleLine (r: range) = r.StartLine = r.EndLine

/// Strip redundant outer parens from an expression whose new context makes
/// them unnecessary (e.g. a lambda body inside our own parenthesized template).
[<TailCall>]
let rec stripParens (e: SynExpr) =
    match e with
    | SynExpr.Paren(expr = inner) -> stripParens inner
    | _ -> e

/// Expressions that need no parentheses when used as a pipe source or a
/// function argument.
let isAtomic (e: SynExpr) =
    match e with
    | SynExpr.Ident _
    | SynExpr.LongIdent _
    | SynExpr.Const _
    | SynExpr.Paren _
    | SynExpr.DotGet _ -> true
    | SynExpr.App(flag = ExprAtomicFlag.Atomic) -> true
    | _ -> false

/// The expression's text, parenthesized unless it is atomic.
let atomicText (source: ISourceText) (e: SynExpr) =
    let text = textOfRange source e.Range
    if isAtomic e then text else $"({text})"

/// The expression's text as a function argument: parenthesized unless atomic,
/// and always parenthesized when it starts with `-` (it would parse as
/// subtraction).
let argumentText (source: ISourceText) (e: SynExpr) =
    let text = textOfRange source e.Range

    if isAtomic e && not (text.StartsWith '-') then
        text
    else
        $"({text})"

/// Pure atoms whose evaluation cannot run user code — safe to evaluate
/// eagerly where the original code evaluated them lazily. Empty collection
/// literals qualify; non-empty ones would evaluate their elements. Dotted
/// paths are deliberately excluded: `DateTime.Now` and friends are property
/// getters whose evaluation is observable.
let isPureAtom (e: SynExpr) =
    match e with
    | SynExpr.Ident _
    | SynExpr.Const _
    | SynExpr.Null _
    | SynExpr.ArrayOrList(exprs = []) -> true
    | _ -> false

/// True when the expression is ordinary expression syntax that can be moved
/// into a lambda body. Computation-expression-only forms (`return e`,
/// `yield e`, `do! e`, `let! ...`) cannot.
let isPlainBody (e: SynExpr) =
    match e with
    | SynExpr.YieldOrReturn _
    | SynExpr.YieldOrReturnFrom _
    | SynExpr.DoBang _ -> false
    | SynExpr.LetOrUse lou -> not lou.IsBang
    | _ -> true

/// An identifier expression, whether parsed as Ident or a single-segment
/// LongIdent (operators like `|>` come through as the latter).
[<return: Struct>]
let (|IdentName|_|) (e: SynExpr) =
    match e with
    | SynExpr.Ident ident -> ValueSome ident.idText
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ ident ])) -> ValueSome ident.idText
    | _ -> ValueNone

/// Like IdentName, but yields the Ident itself (for symbol resolution).
[<return: Struct>]
let (|SingleIdent|_|) (e: SynExpr) =
    match e with
    | SynExpr.Ident ident -> ValueSome ident
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ ident ])) -> ValueSome ident
    | _ -> ValueNone

/// True when the text ends in a printf format specifier whose leading `%`
/// is not itself escaped (`%%`) — the shape a typed interpolation hole has
/// in the literal part preceding its `{`.
let endsWithFormatSpecifier (text: string) =
    System.Text.RegularExpressions.Regex.IsMatch(text, @"%[-+0# ]*[0-9]*(\.[0-9]+)?[a-zA-Z]$")
    && (let idx = text.LastIndexOf '%'
        let mutable run = 0
        let mutable i = idx - 1

        while i >= 0 && text.[i] = '%' do
            run <- run + 1
            i <- i - 1

        run % 2 = 0)

[<return: Struct>]
let (|BoolConst|_|) (e: SynExpr) =
    match e with
    | SynExpr.Const(SynConst.Bool b, _) -> ValueSome b
    | _ -> ValueNone

[<return: Struct>]
let (|UnitConst|_|) (e: SynExpr) =
    match e with
    | SynExpr.Const(SynConst.Unit, _) -> ValueSome()
    | _ -> ValueNone

[<return: Struct>]
let (|ZeroConst|_|) (e: SynExpr) =
    match e with
    | SynExpr.Const(SynConst.Int32 0, _) -> ValueSome()
    | _ -> ValueNone

/// `lhs |> rhs` (the parsed shape of the infix pipe).
[<return: Struct>]
let (|PipeApp|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = IdentName "op_PipeRight"; argExpr = lhs); argExpr = rhs) ->
        ValueSome(lhs, rhs)
    | _ -> ValueNone

/// A guard-free match clause's pattern and body.
let simpleClause (SynMatchClause(pat = pat; whenExpr = whenExpr; resultExpr = result)) =
    match whenExpr with
    | Some _ -> None
    | None -> Some(pat, result)

/// The name a case pattern binds: `v`, `(v)`, or None for `_`.
[<TailCall>]
let rec boundVar (arg: SynPat) =
    match arg with
    | SynPat.Named(ident = SynIdent(ident = v)) -> Some(Some v.idText)
    | SynPat.Wild _ -> Some None
    | SynPat.Paren(pat = inner) -> boundVar inner
    | _ -> None

/// Lambda parameter name for an optionally-bound variable.
let lambdaParam (boundVar: string option) = defaultArg boundVar "_"

/// Every name bound anywhere in a pattern (loop patterns, lambda
/// parameters, constructor arguments).
[<TailCall>]
let rec patBoundNamesLoop (acc: string list) (pending: SynPat list) =
    match pending with
    | [] -> acc
    | p :: rest ->
        let acc, next =
            match p with
            | SynPat.Named(ident = SynIdent(ident = id)) -> id.idText :: acc, rest
            | SynPat.Typed(pat = inner)
            | SynPat.Attrib(pat = inner)
            | SynPat.Paren(inner, _) -> acc, inner :: rest
            | SynPat.Tuple(elementPats = ps)
            | SynPat.ArrayOrList(elementPats = ps)
            | SynPat.Ands(pats = ps) -> acc, ps @ rest
            | SynPat.As(lhsPat = l; rhsPat = r)
            | SynPat.Or(lhsPat = l; rhsPat = r) -> acc, l :: r :: rest
            | SynPat.LongIdent(argPats = SynArgPats.Pats ps) -> acc, ps @ rest
            | _ -> acc, rest

        patBoundNamesLoop acc next

let patBoundNames (p: SynPat) : string list = patBoundNamesLoop [] [ p ]

/// True when the expression's source text can be inlined at an arbitrary
/// single-line expression position without changing how it parses. Anything
/// greedy enough to swallow following tokens (`else`, `then`, a pipeline
/// stage), or that needs `;`/`in` on one line, is rejected; parenthesized
/// expressions are always fine.
[<TailCall>]
let rec isSafeInline (e: SynExpr) : bool =
    match e with
    | SynExpr.Paren _ -> true
    | SynExpr.IfThenElse _
    | SynExpr.Match _
    | SynExpr.MatchLambda _
    | SynExpr.MatchBang _
    | SynExpr.Lambda _
    | SynExpr.LetOrUse _
    | SynExpr.Sequential _
    | SynExpr.TryWith _
    | SynExpr.TryFinally _
    | SynExpr.Do _
    | SynExpr.DoBang _
    | SynExpr.While _
    | SynExpr.For _
    | SynExpr.ForEach _ -> false
    // An application is only as safe as its rightmost part:
    // `f <| fun x -> x` ends in an unparenthesized lambda.
    | SynExpr.App(argExpr = arg) -> isSafeInline arg
    | SynExpr.Tuple(exprs = exprs) ->
        match exprs with
        | [] -> true
        | _ -> isSafeInline (List.last exprs)
    | SynExpr.Typed(expr = inner) -> isSafeInline inner
    | _ -> true
