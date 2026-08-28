/// Refactoring: in an interpolated string that already uses typed holes,
/// type the remaining plain holes too.
///
///     $"%s{name} is {age}"   →  $"%s{name} is %d{age}"
///
/// A typed hole pins the fill's type at compile time — change `age` to a
/// record and `%d{age}` stops compiling where `{age}` silently switches
/// to ToString output.
///
/// Deliberately narrow:
///   - only strings that ALREADY contain a %-specifier hole are touched:
///     those are on the printf formatting path anyway, so adding
///     specifiers costs nothing. A specifier-free string interpolation
///     lowers to String.Concat on F# 8+ — adding `%s` there would move it
///     to the slower path, so it is left alone.
///   - only specifiers whose output provably equals ToString: `%s` for
///     strings, `%d` for integer types, `%c` for chars. `%b` lowercases
///     booleans and `%f` pads floats — those fills stay untyped.
///   - the fill must be an identifier or dotted path that resolves via
///     the typed check results.
module FSharp.Refactor.TypedHoles

open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        /// Zero-width insertion point just before the fill's `{`.
        Range: range
        /// "%s", "%d", or "%c".
        Specifier: string
        /// The fill's text, for the message.
        FillText: string
    }

/// Trailing text that means the NEXT fill already has a specifier.
let private hasUnescapedSpecifier (text: string) = endsWithFormatSpecifier text

let private integerTypes =
    set
        [ "System.Int32"
          "System.Int64"
          "System.Int16"
          "System.SByte"
          "System.Byte"
          "System.UInt16"
          "System.UInt32"
          "System.UInt64" ]

/// The provably ToString-identical specifier for the fill's type.
let private specifierFor (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        let fillType =
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as value ->
                Some(
                    try
                        value.ReturnParameter.Type
                    with _ ->
                        value.FullType
                )
            | :? FSharpField as field -> Some field.FieldType
            | _ -> None

        fillType
        |> Option.bind (fun t ->
            try
                let t = OptionModule.stripAbbreviations t

                if not t.HasTypeDefinition then
                    None
                else
                    match t.TypeDefinition.TryFullName with
                    | Some "System.String" -> Some "%s"
                    | Some "System.Char" -> Some "%c"
                    | Some name when integerTypes.Contains name -> Some "%d"
                    | _ -> None
            with OptionModule.FcsSymbolFailure ->
                None)
    | None -> None

/// Find untyped fills in already-typed interpolated strings. Requires
/// typed check results.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.InterpolatedString(contents = parts) ->
                  // pair every fill with the literal text preceding it
                  let fillsWithLeadText =
                      parts
                      |> List.pairwise
                      |> List.choose (fun pair ->
                          match pair with
                          | SynInterpolatedStringPart.String(value = lead; range = leadRange),
                            SynInterpolatedStringPart.FillExpr(fillExpr = fill; qualifiers = None) ->
                              Some(lead, leadRange, fill)
                          | _ -> None)

                  let anyTyped =
                      fillsWithLeadText
                      |> List.exists (fun (lead, _, _) -> hasUnescapedSpecifier lead)

                  if anyTyped then
                      for lead, leadRange, fill in fillsWithLeadText do
                          if not (hasUnescapedSpecifier lead) then
                              let fillIdent =
                                  match stripParens fill with
                                  | SynExpr.Ident id -> Some id
                                  | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty ->
                                      Some(List.last ids)
                                  | _ -> None

                              match fillIdent |> Option.bind (specifierFor check source) with
                              | Some specifier when leadRange.EndColumn >= 1 ->
                                  // the String part's range includes the
                                  // trailing `{`; insert just before it
                                  let insertAt = Position.mkPos leadRange.EndLine (leadRange.EndColumn - 1)

                                  { Range = Range.mkRange leadRange.FileName insertAt insertAt
                                    Specifier = specifier
                                    FillText = textOfRange source fill.Range }
                              | _ -> ()
              | _ -> () ]
