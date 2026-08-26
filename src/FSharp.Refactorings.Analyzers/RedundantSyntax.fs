/// Four ReSharper-tradition redundancy fixes:
///
/// 1. Attribute suffix (FR0082): `[<SerializableAttribute>]` →
///    `[<Serializable>]` — the compiler resolves the short form.
/// 2. Attribute parens (FR0083): `[<Foo()>]` → `[<Foo>]` — an empty
///    argument list on an attribute says nothing.
/// 3. Redundant backticks (FR0084): ``` ``name`` ``` where `name` is a
///    plain identifier and not a keyword — the quoting does nothing, at
///    this use site independently of any other.
/// 4. Hole-free interpolation (FR0086): `$"just text"` → `"just text"` —
///    without fills the `$` only costs reader attention. Skipped when the
///    text contains braces (they would need unescaping from `{{`/`}}`).
module FSharp.Refactorings.RedundantSyntax

open System.Text.RegularExpressions
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

[<RequireQualifiedAccess>]
type Kind =
    | AttributeSuffix
    | AttributeParens
    | Backticks
    | HoleFreeInterpolation

type Suggestion =
    { Range: range
      OriginalText: string
      ReplacementText: string
      Kind: Kind }

let private plainIdent = Regex(@"^[A-Za-z_][A-Za-z0-9_']*$", RegexOptions.Compiled)

let private keywords =
    Set.ofList FSharp.Compiler.Tokenization.FSharpKeywords.KeywordNames

/// A backtick-quoted ident whose quoting does nothing. An underscore-only
/// name stays quoted: bare `_` is the wildcard, not a binder.
let private redundantBackticks (source: ISourceText) (ident: Ident) =
    plainIdent.IsMatch ident.idText
    && ident.idText.TrimStart '_' <> ""
    && not (keywords.Contains ident.idText)
    && isSingleLine ident.idRange
    && textOfRange source ident.idRange = $"``{ident.idText}``"

let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let index = AstIndex.ofTree parseTree
    let suggestions = ResizeArray<Suggestion>()

    // a type declared in this file under the SHORT name would win the
    // attribute lookup after trimming (attribute resolution tries the
    // exact name before appending "Attribute")
    let fileTypeNames =
        index.Decls
        |> Array.collect (fun (_, decl) ->
            match decl with
            | SynModuleDecl.Types(typeDefns = defns) ->
                defns
                |> List.choose (fun (SynTypeDefn(typeInfo = SynComponentInfo(longId = ids))) ->
                    ids |> List.tryLast |> Option.map (fun i -> i.idText))
                |> Array.ofList
            | _ -> [||])
        |> Set.ofArray

    for _, attr in index.Attributes do
        // FR0082: the Attribute suffix
        match attr.TypeName with
        | SynLongIdent(id = ids) when not ids.IsEmpty ->
            let last = List.last ids
            let text = last.idText

            if
                text.EndsWith "Attribute"
                && text.Length > "Attribute".Length
                && not (fileTypeNames.Contains(text.Substring(0, text.Length - "Attribute".Length)))
                && textOfRange source last.idRange = text
            then
                suggestions.Add
                    { Range = last.idRange
                      OriginalText = text
                      ReplacementText = text.Substring(0, text.Length - "Attribute".Length)
                      Kind = Kind.AttributeSuffix }
        | _ -> ()

        // FR0083: the empty argument list
        match attr.ArgExpr with
        | SynExpr.Const(SynConst.Unit, unitRange) when textOfRange source unitRange = "()" ->
            suggestions.Add
                { Range = unitRange
                  OriginalText = "()"
                  ReplacementText = ""
                  Kind = Kind.AttributeParens }
        | _ -> ()

    // FR0084: redundant backticks at use sites and binder sites — each
    // strip is independently valid, backticks are optional quoting
    for _, e in index.Exprs do
        match e with
        | SynExpr.Ident ident when redundantBackticks source ident ->
            suggestions.Add
                { Range = ident.idRange
                  OriginalText = textOfRange source ident.idRange
                  ReplacementText = ident.idText
                  Kind = Kind.Backticks }
        | SynExpr.InterpolatedString(contents = parts) when
            parts
            |> List.forall (fun p ->
                match p with
                | SynInterpolatedStringPart.String _ -> true
                | SynInterpolatedStringPart.FillExpr _ -> false)
            ->
            // FR0086: no holes — drop the `$` unless braces would need
            // unescaping, or a `%%` its un-doubling ('%' escapes in
            // interpolated strings but not in plain ones)
            let text = textOfRange source e.Range

            if
                not (text.Contains '{' || text.Contains '}' || text.Contains '%')
                && text.Contains '$'
            then
                suggestions.Add
                    { Range = e.Range
                      OriginalText = text
                      ReplacementText = text.Remove(text.IndexOf '$', 1)
                      Kind = Kind.HoleFreeInterpolation }
        | _ -> ()

    for _, p in index.Pats do
        match p with
        | SynPat.Named(ident = SynIdent(ident = ident)) when redundantBackticks source ident ->
            suggestions.Add
                { Range = ident.idRange
                  OriginalText = textOfRange source ident.idRange
                  ReplacementText = ident.idText
                  Kind = Kind.Backticks }
        | _ -> ()

    List.ofSeq suggestions
