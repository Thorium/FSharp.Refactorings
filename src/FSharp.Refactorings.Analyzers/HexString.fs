/// Refactoring (performance, CA1872): hex encoding through BitConverter
/// allocates the dashed string only to strip it again.
///
///     BitConverter.ToString(bytes).Replace("-", "")
///         →  System.Convert.ToHexString bytes
///
/// Convert.ToHexString produces the identical uppercase hex directly (one
/// allocation instead of three). Only the exact dash-stripping shape over
/// a single-argument ToString is rewritten; the offset/length overloads
/// and other Replace arguments are left alone.
module FSharp.Refactorings.HexString

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

type Suggestion =
    { Range: range
      OriginalText: string
      ReplacementText: string }

/// `BitConverter.ToString(<single arg>)`, optionally System-qualified.
[<return: Struct>]
let private (|BitConverterToString|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = arg) when
        ids.Length >= 2
        && (List.last ids).idText = "ToString"
        && ids.[ids.Length - 2].idText = "BitConverter"
        ->
        match stripParens arg with
        | SynExpr.Tuple _ -> ValueNone // offset/length overloads
        | single -> ValueSome single
    | _ -> ValueNone

[<return: Struct>]
let private (|StringConst|_|) (e: SynExpr) =
    match e with
    | SynExpr.Const(SynConst.String(text, _, _), _) -> ValueSome text
    | _ -> ValueNone

/// Does this compilation's reference set include Convert.ToHexString?
/// (.NET 5+ — absent on netstandard2.0/net48, where the fix would not
/// compile.)
let private toHexStringAvailable (check: FSharpCheckFileResults) =
    check.ProjectContext.GetReferencedAssemblies()
    |> Seq.exists (fun assembly ->
        try
            match assembly.Contents.FindEntityByPath [ "System"; "Convert" ] with
            | Some entity ->
                entity.MembersFunctionsAndValues
                |> Seq.exists (fun m -> m.LogicalName = "ToHexString")
            | None -> false
        with _ ->
            false)

/// Find dash-stripped BitConverter hex chains. Requires typed check results
/// for the target-framework gate.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    let candidates =
        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.App(
                  isInfix = false
                  funcExpr = SynExpr.DotGet(
                      expr = BitConverterToString bytes; longDotId = SynLongIdent(id = [ replaceId ]))
                  argExpr = arg) when replaceId.idText = "Replace" && isSingleLine expr.Range ->
                  match stripParens arg with
                  | SynExpr.Tuple(exprs = [ StringConst "-"; StringConst "" ]) ->
                      { Range = expr.Range
                        OriginalText = textOfRange source expr.Range
                        ReplacementText = $"System.Convert.ToHexString {argumentText source bytes}" }
                  | _ -> ()
              | _ -> () ]

    // gate only when there is something to gate: the assembly scan is the
    // expensive part
    match candidates with
    | [] -> []
    | found -> if toHexStringAvailable check then found else []
