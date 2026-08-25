/// Refactoring (performance, CA1834/CA1847/CA1865-1867 family): a
/// single-character string passed where a char overload exists.
///
///     s.Contains "x"                            →  s.Contains 'x'
///     sb.Append "x"                             →  sb.Append 'x'
///     s.StartsWith("x", StringComparison.Ordinal)  →  s.StartsWith 'x'
///
/// The char overloads skip the string-comparison setup entirely.
///
/// The culture nuance decides fix vs advice: `Contains(string)` and
/// `StringBuilder.Append` are already ordinal, and an explicit
/// `StringComparison.Ordinal` argument matches the char overload's
/// behavior — those get fixes. Bare `StartsWith`/`EndsWith`/`IndexOf`
/// with a string are CULTURE-sensitive while the char overloads are
/// ordinal, so those only get an advisory note asking the author to
/// verify ordinal semantics are acceptable.
///
/// Method resolution is typed-gated to System.String / StringBuilder, so
/// a Contains on a collection never matches.
module FSharp.Refactorings.CharOverload

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

type Suggestion =
    {
        Range: range
        OriginalText: string
        /// None = advisory only (culture-sensitive overload).
        ReplacementText: string option
        MethodName: string
    }

/// string methods with char overloads that are ordinal either way
let private ordinalSafeMethods = set [ "Contains" ]

/// string methods whose string overload is culture-sensitive
let private cultureSensitiveMethods =
    set [ "StartsWith"; "EndsWith"; "IndexOf"; "LastIndexOf" ]

let private enclosingEntities =
    [ "System.String", ordinalSafeMethods + cultureSensitiveMethods
      "System.Text.StringBuilder", set [ "Append" ] ]

/// Render a char literal for the single character of a string constant.
let private charLiteral (c: char) =
    let escaped =
        match c with
        | '\\' -> "\\\\"
        | '\'' -> "\\'"
        | '\n' -> "\\n"
        | '\r' -> "\\r"
        | '\t' -> "\\t"
        | other ->
            if System.Char.IsControl other then
                sprintf "\\u%04x" (int other)
            else
                string other

    $"'{escaped}'"

/// A single-character regular string constant.
[<return: Struct>]
let private (|SingleCharString|_|) (e: SynExpr) =
    match e with
    | SynExpr.Const(SynConst.String(text, SynStringKind.Regular, _), _) when text.Length = 1 ->
        ValueSome(text.[0], e.Range)
    | _ -> ValueNone

/// `StringComparison.Ordinal` as an argument expression.
[<return: Struct>]
let private (|OrdinalComparison|_|) (e: SynExpr) =
    match e with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
        ids.Length >= 2
        && (List.last ids).idText = "Ordinal"
        && ids.[ids.Length - 2].idText = "StringComparison"
        ->
        ValueSome()
    | _ -> ValueNone

/// Does the method identifier resolve to one of the gated BCL types?
let private resolvesToGatedMethod (check: FSharpCheckFileResults) (source: ISourceText) (methodId: Ident) =
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

            enclosingEntities
            |> List.exists (fun (entity, methods) -> enclosing = entity && methods.Contains methodId.idText)
        | _ -> false
    | None -> false

/// Find single-character string arguments to char-overloaded methods.
/// Requires typed check results for the receiver gate.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.App(isInfix = false; funcExpr = funcExpr; argExpr = arg) ->
                  let methodId =
                      match funcExpr with
                      | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when ids.Length >= 2 ->
                          Some(List.last ids)
                      | SynExpr.DotGet(longDotId = SynLongIdent(id = [ id ])) -> Some id
                      | _ -> None

                  match methodId with
                  | Some methodId when
                      (ordinalSafeMethods.Contains methodId.idText
                       || cultureSensitiveMethods.Contains methodId.idText
                       || methodId.idText = "Append")
                      && resolvesToGatedMethod check source methodId
                      ->
                      let cultureSensitive = cultureSensitiveMethods.Contains methodId.idText

                      match stripParens arg with
                      | SingleCharString(c, literalRange) ->
                          // bare culture-sensitive calls get advice only;
                          // ordinal-safe ones get the literal replaced
                          let range, replacement =
                              if cultureSensitive then
                                  expr.Range, None
                              else
                                  literalRange, Some(charLiteral c)

                          { Range = range
                            OriginalText = textOfRange source range
                            ReplacementText = replacement
                            MethodName = methodId.idText }
                      | SynExpr.Tuple(exprs = [ SingleCharString(c, _); OrdinalComparison ]) when cultureSensitive ->
                          // explicit Ordinal matches the char overload: fix
                          // by replacing the whole argument list
                          { Range = arg.Range
                            OriginalText = textOfRange source arg.Range
                            ReplacementText = Some $"({charLiteral c})"
                            MethodName = methodId.idText }
                      | _ -> ()
                  | _ -> ()
              | _ -> () ]
