/// Refactoring note (performance): on .NET 8+ System.Linq's Sum, Average,
/// Min, and Max are SIMD-vectorized over primitive arrays, while F#'s
/// Array.sum family is a scalar loop — for large int[]/int64[] the LINQ
/// call is several times faster.
///
///     values |> Array.sum      →  values.Sum()   // open System.Linq
///
/// Advice, not a fix, because the semantics are not identical:
///   - LINQ Sum is overflow-CHECKED (throws OverflowException) where
///     Array.sum wraps silently — usually an improvement, but a change
///   - on floats, F# min/max and LINQ Min/Max disagree about NaN, so the
///     note is gated to int/int64 element types where the win is clean
///
/// The array value must be a plain identifier whose type resolves (typed
/// check results) to a vectorizable primitive array.
module FSharp.Refactorings.VectorizedLinq

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

type Suggestion =
    {
        Range: range
        /// "sum", "average", "min", "max" — for the message.
        FunctionName: string
        /// The array value's name, for the message.
        ArrayName: string
    }

let private vectorizedFunctions = set [ "sum"; "average"; "min"; "max" ]

/// Element types whose LINQ aggregations are vectorized with clean
/// semantics (floats excluded: NaN handling differs between the worlds).
let private vectorizedElements = set [ "System.Int32"; "System.Int64" ]

/// `Array.<fn> arr` / `arr |> Array.<fn>` for an aggregation function.
[<return: Struct>]
let private (|ArrayAggregation|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(
        isInfix = false
        funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ]))
        argExpr = SynExpr.Ident arr) when m.idText = "Array" && vectorizedFunctions.Contains f.idText ->
        ValueSome(f.idText, arr)
    | PipeApp(SynExpr.Ident arr, SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ]))) when
        m.idText = "Array" && vectorizedFunctions.Contains f.idText
        ->
        ValueSome(f.idText, arr)
    | _ -> ValueNone

[<TailCall>]
let rec private stripAbbreviations (t: FSharpType) =
    if t.HasTypeDefinition && t.TypeDefinition.IsFSharpAbbreviation then
        stripAbbreviations t.TypeDefinition.AbbreviatedType
    else
        t

/// Does the identifier resolve to an int[]/int64[]?
let private resolvesToVectorizableArray (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value ->
            try
                let t = stripAbbreviations value.FullType

                t.HasTypeDefinition
                && t.TypeDefinition.IsArrayType
                && t.GenericArguments.Count = 1
                && (stripAbbreviations t.GenericArguments.[0]).TypeDefinition.TryFullName
                   |> Option.exists vectorizedElements.Contains
            with _ ->
                false
        | _ -> false
    | None -> false

/// Find scalar Array aggregations with a vectorized LINQ equivalent.
/// Requires typed check results.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for _, expr in index.Exprs do
              match expr with
              | ArrayAggregation(fn, arr) when resolvesToVectorizableArray check source arr ->
                  { Range = expr.Range
                    FunctionName = fn
                    ArrayName = arr.idText }
              | _ -> () ]
