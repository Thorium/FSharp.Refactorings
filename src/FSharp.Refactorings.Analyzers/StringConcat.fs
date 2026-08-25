/// Refactoring: a `+` chain mixing string literals and string values is an
/// interpolated string.
///
///     "Hello " + name + "!"       →  $"Hello {name}!"
///     prefix + ": " + x.Label     →  $"{prefix}: {x.Label}"
///
/// Safety rules:
///   - every operand is a regular string literal, or an identifier/dotted
///     path that resolves (typed check results) to System.String — any
///     other operand shape leaves the chain alone, so a custom (+) can
///     never be rewritten
///   - at least one literal and one non-literal (an all-literal chain is a
///     different simplification; a literal-free chain gains nothing)
///   - a literal containing `{`, `}`, or `%` leaves the chain alone: the
///     doubled escapes an interpolated string would need (`{{`, `%%`) read
///     worse than the concatenation they replace
///
/// Performance note: F# 8+ lowers a specifier-free interpolation with
/// string-typed holes — the only shape this rule emits — to a single n-ary
/// String.Concat, so the rewrite never routes through String.Format and is
/// equal to or cheaper than the pairwise `+` chain it replaces.
module FSharp.Refactorings.StringConcat

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

type Suggestion =
    { Range: range
      OriginalText: string
      ReplacementText: string }

/// One operand of the chain: literal source text or an interpolation hole.
type private Piece =
    | Lit of string
    | Hole of string

/// Left-to-right operands of a `+` chain.
[<TailCall>]
let rec private collectOperands (acc: SynExpr list) (e: SynExpr) =
    match e with
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = IdentName "op_Addition"; argExpr = lhs); argExpr = rhs) ->
        collectOperands (rhs :: acc) lhs
    | last -> last :: acc

[<TailCall>]
let rec private stripAbbreviations (t: FSharpType) =
    if t.HasTypeDefinition && t.TypeDefinition.IsFSharpAbbreviation then
        stripAbbreviations t.TypeDefinition.AbbreviatedType
    else
        t

/// Does the identifier resolve to a System.String value or property?
let private resolvesToString (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    let isStringType (t: FSharpType) =
        try
            let t = stripAbbreviations t
            t.HasTypeDefinition && t.TypeDefinition.TryFullName = Some "System.String"
        with _ ->
            false

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value ->
            isStringType (
                try
                    value.ReturnParameter.Type
                with _ ->
                    value.FullType
            )
        // record fields resolve as FSharpField, not as a value
        | :? FSharpField as field -> isStringType field.FieldType
        | _ -> false
    | None -> false

/// Literal source text with the surrounding quotes stripped, or None when
/// the interpolated context would force awkward `{{`/`%%` escapes.
let private spliceableLiteral (literalSource: string) =
    let inner = literalSource.Substring(1, literalSource.Length - 2)

    if inner.Contains '{' || inner.Contains '}' || inner.Contains '%' then
        None
    else
        Some inner

/// True when the walker node is an operand of an enclosing `+`, i.e. an
/// inner node of a chain the outermost visit already handles.
let private isOperandOfPlus (path: SyntaxNode list) =
    match path with
    | SyntaxNode.SynExpr(SynExpr.App(funcExpr = IdentName "op_Addition")) :: _ -> true
    | SyntaxNode.SynExpr(SynExpr.App(funcExpr = SynExpr.App(funcExpr = IdentName "op_Addition"))) :: _ -> true
    | _ -> false

/// Find rewritable concatenation chains. Requires typed check results.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for path, expr in index.Exprs do
              match expr with
              | SynExpr.App(funcExpr = SynExpr.App(funcExpr = SingleIdent opId; argExpr = _); argExpr = _) when
                  opId.idText = "op_Addition"
                  && not (isOperandOfPlus path)
                  && isSingleLine expr.Range
                  // a shadowed (+) can have arbitrary semantics
                  && OptionModule.resolvesToCoreOperator check source opId
                  ->
                  let operands = collectOperands [] expr

                  let pieces =
                      operands
                      |> List.map (fun operand ->
                          match operand with
                          | SynExpr.Const(SynConst.String(_, SynStringKind.Regular, _), _) ->
                              spliceableLiteral (textOfRange source operand.Range) |> Option.map Lit
                          | SynExpr.Ident id when resolvesToString check source id -> Some(Hole id.idText)
                          | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
                              not ids.IsEmpty && resolvesToString check source (List.last ids)
                              ->
                              Some(Hole(textOfRange source operand.Range))
                          | _ -> None)

                  let hasLit =
                      pieces
                      |> List.exists (fun p ->
                          match p with
                          | Some(Lit _) -> true
                          | _ -> false)

                  let hasHole =
                      pieces
                      |> List.exists (fun p ->
                          match p with
                          | Some(Hole _) -> true
                          | _ -> false)

                  if pieces |> List.forall Option.isSome && hasLit && hasHole then
                      let body =
                          pieces
                          |> List.choose id
                          |> List.map (fun piece ->
                              match piece with
                              | Lit text -> text
                              | Hole text -> "{" + text + "}")
                          |> String.concat ""

                      { Range = expr.Range
                        OriginalText = textOfRange source expr.Range
                        ReplacementText = $"$\"{body}\"" }
              | _ -> () ]
