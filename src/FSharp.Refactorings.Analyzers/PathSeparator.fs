/// Refactoring note (portability): concatenating path fragments with a
/// hard-coded separator builds the path by hand.
///
///     dir + "\\" + file          // wrong separator off Windows
///     root + "/" + sub + "/" + f
///         →  Path.Combine(dir, file) handles separators, duplicates,
///            and platform differences
///
/// Advice only, deliberately:
///   - Path.Combine treats a ROOTED second argument as absolute (the
///     first is discarded) — the concatenation does not, so the rewrite
///     is not universally identical
///   - a URL is not a file path: forward-slash joins on anything smelling
///     of URLs (url/uri/http/route/endpoint/link/href, or a literal
///     containing "://") never fire, since Path.Combine would produce
///     backslashes on Windows
module FSharp.Refactorings.PathSeparator

open System.Text.RegularExpressions
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Refactorings.Text

type Suggestion =
    {
        Range: range
        /// "/" or "\\", for the message.
        Separator: string
    }

/// Left-to-right operands of a `+` chain.
[<TailCall>]
let rec private plusOperandsLoop (acc: SynExpr list) (e: SynExpr) =
    match e with
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = SingleIdent op; argExpr = lhs); argExpr = rhs) when
        op.idText = "op_Addition"
        ->
        plusOperandsLoop (rhs :: acc) lhs
    | leaf -> leaf :: acc

let private urlSmell =
    Regex(@"(?i)url|uri|http|link|route|endpoint|href|slug|query", RegexOptions.Compiled)

/// Positive path evidence, required for forward-slash joins ('\' is
/// path-ish on its own): path-flavored identifiers, the classic directory
/// sources (__SOURCE_DIRECTORY__, Environment.CurrentDirectory, assembly
/// locations, AppContext.BaseDirectory), a rooted or extension-bearing
/// literal — or a literal that actually EXISTS on this machine, the
/// strongest signal there is.
let private pathSmell =
    Regex(
        @"(?i)path|dir|file|folder|root|temp|home|cache|log|__SOURCE_DIRECTORY__|CurrentDirectory|BaseDirectory|GetEntryAssembly|GetExecutingAssembly|\.Location",
        RegexOptions.Compiled
    )

let private rootedLiteral =
    Regex(@"^([A-Za-z]:[\\/]|\\\\|~[\\/]|\.{1,2}[\\/])", RegexOptions.Compiled)

let private extensionLiteral = Regex(@"\.\w{1,5}$", RegexOptions.Compiled)

let private existsOnDisk (text: string) =
    try
        rootedLiteral.IsMatch text
        && (System.IO.Directory.Exists text || System.IO.File.Exists text)
    with _ ->
        false

/// A string literal that is a separator or starts/ends with one.
let private separatorOf (text: string) =
    if text = "/" || text.StartsWith '/' || text.EndsWith '/' then
        Some "/"
    elif text = "\\" || text.StartsWith '\\' || text.EndsWith '\\' then
        Some "\\"
    else
        None

let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    // contexts that must stay COMPILE-TIME literals — Path.Combine is a
    // function call and cannot appear there: [<Literal>] binding bodies,
    // attribute arguments, and type-provider static arguments
    let literalOnlyRanges =
        [ for _, decl in index.Decls do
              match decl with
              | SynModuleDecl.Let(bindings = bindings) ->
                  for SynBinding(attributes = attrs; expr = body) in bindings do
                      if hasAttributeNamed "Literal" attrs then
                          yield body.Range
              | _ -> ()
          for _, attr in index.Attributes -> attr.ArgExpr.Range ]

    let mustStayLiteral (path: SyntaxNode list) (r: range) =
        literalOnlyRanges |> List.exists (fun lr -> Range.rangeContainsRange lr r)
        || path
           |> List.exists (fun node ->
               match node with
               | SyntaxNode.SynType _ -> true // a type-provider static arg
               | _ -> false)

    [ for path, expr in index.Exprs do
          match expr with
          | SynExpr.App(funcExpr = SynExpr.App(funcExpr = SingleIdent op; argExpr = _); argExpr = _) when
              op.idText = "op_Addition"
              && isSingleLine expr.Range
              // outermost chain node only
              && (match path with
                  | SyntaxNode.SynExpr(SynExpr.App(funcExpr = SynExpr.App(funcExpr = IdentName "op_Addition"))) :: _
                  | SyntaxNode.SynExpr(SynExpr.App(funcExpr = IdentName "op_Addition")) :: _ -> false
                  | _ -> true)
              ->
              let operands = plusOperandsLoop [] expr

              let literalTexts =
                  operands
                  |> List.choose (fun o ->
                      match o with
                      | SynExpr.Const(SynConst.String(text, SynStringKind.Regular, _), _) -> Some text
                      | _ -> None)

              let separators = literalTexts |> List.choose separatorOf |> List.distinct

              // an inner separator literal joining non-literal parts,
              // nothing URL-ish anywhere in the chain
              let smellsOfUrl =
                  literalTexts |> List.exists (fun t -> t.Contains "://")
                  || urlSmell.IsMatch(textOfRange source expr.Range)

              let hasNonLiteralPart =
                  operands
                  |> List.exists (fun o ->
                      match o with
                      // __SOURCE_DIRECTORY__ parses as a Const, but it IS
                      // the joined-onto directory
                      | SynExpr.Const(SynConst.SourceIdentifier _, _) -> true
                      | SynExpr.Const _ -> false
                      | _ -> true)

              // '\' joins are path-ish on their own; '/' joins also serve
              // prose ("cats / dogs") and URLs, so they need positive
              // evidence: path-flavored names, a rooted or
              // extension-bearing literal, or a literal existing on disk
              let hasPathEvidence (separator: string) =
                  separator = "\\"
                  || pathSmell.IsMatch(textOfRange source expr.Range)
                  || literalTexts
                     |> List.exists (fun t -> rootedLiteral.IsMatch t || extensionLiteral.IsMatch t || existsOnDisk t)

              match separators with
              | [ separator ] when
                  hasNonLiteralPart
                  && not smellsOfUrl
                  && hasPathEvidence separator
                  && not (mustStayLiteral path expr.Range)
                  ->
                  { Range = expr.Range
                    Separator = separator }
              | _ -> ()
          | _ -> () ]
