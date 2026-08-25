/// Refactoring note (performance, CA1862): comparing via ToLower/ToUpper
/// allocates a lowered copy of the string just to throw it away.
///
///     a.ToLower() = b.ToLower()        // two allocations per comparison
///     s.ToLower().StartsWith "abc"     // one allocation per call
///
/// The allocation-free spellings are `String.Equals(a, b, comparison)` and
/// the `StringComparison` overloads of Contains/StartsWith/EndsWith/
/// IndexOf. This is advice, not a fix: lower-then-compare, OrdinalIgnoreCase
/// and CultureIgnoreCase can differ on edge cases (Turkish dotless i, ß),
/// so the comparison type is the author's deliberate choice.
///
/// The lowering method is typed-gated to System.String.
module FSharp.Refactorings.CaseInsensitive

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

[<RequireQualifiedAccess>]
type CaseKind =
    /// `a.ToLower() = ...` — suggest String.Equals with a comparison.
    | Equality
    /// `s.ToLower().Contains ...` — suggest the comparison overload.
    | MethodCall of methodName: string

type Suggestion =
    {
        Range: range
        Kind: CaseKind
        /// The lowering method used, for the message.
        LoweringName: string
    }

let private loweringMethods =
    set [ "ToLower"; "ToUpper"; "ToLowerInvariant"; "ToUpperInvariant" ]

let private comparisonMethods =
    set [ "Contains"; "StartsWith"; "EndsWith"; "IndexOf"; "LastIndexOf" ]

/// `<receiver>.ToLower()` and friends — the lowering method identifier.
[<return: Struct>]
let private (|LoweredCall|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = UnitConst) when
        ids.Length >= 2 && loweringMethods.Contains (List.last ids).idText
        ->
        ValueSome(List.last ids)
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.DotGet(longDotId = SynLongIdent(id = [ m ])); argExpr = UnitConst) when
        loweringMethods.Contains m.idText
        ->
        ValueSome m
    | _ -> ValueNone

/// Does the lowering method resolve to System.String?
let private resolvesToStringMethod (check: FSharpCheckFileResults) (source: ISourceText) (methodId: Ident) =
    let r = methodId.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ methodId.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value ->
            let enclosing =
                try
                    value.ApparentEnclosingEntity
                    |> Option.bind (fun e -> e.TryFullName)
                    |> Option.defaultValue ""
                with _ ->
                    ""

            enclosing = "System.String"
        | _ -> false
    | None -> false

/// Find allocating case-insensitive comparisons. Requires typed check
/// results for the string gate.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.App(
                  funcExpr = SynExpr.App(funcExpr = IdentName("op_Equality" | "op_Inequality"); argExpr = lhs)
                  argExpr = rhs) ->
                  let lowered =
                      match stripParens lhs, stripParens rhs with
                      | LoweredCall m, _
                      | _, LoweredCall m -> Some m
                      | _ -> None

                  match lowered with
                  | Some m when resolvesToStringMethod check source m ->
                      { Range = expr.Range
                        Kind = CaseKind.Equality
                        LoweringName = m.idText }
                  | _ -> ()
              | SynExpr.App(
                  isInfix = false
                  funcExpr = SynExpr.DotGet(expr = LoweredCall lowering; longDotId = SynLongIdent(id = [ methodId ]))) when
                  comparisonMethods.Contains methodId.idText
                  ->
                  if resolvesToStringMethod check source lowering then
                      { Range = expr.Range
                        Kind = CaseKind.MethodCall methodId.idText
                        LoweringName = lowering.idText }
              | _ -> () ]
