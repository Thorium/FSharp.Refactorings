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

              // A separator only JOINS when the chain has something on both
              // sides of it. `Path.GetFileName d + "/"` appends a trailing
              // marker and `"/" + name` prefixes a root — neither is a
              // Path.Combine, and Path.Combine cannot even express the
              // first. An inner literal always joins; an outer one has to
              // carry text on the far side of its separator, so
              // `dir + "/file.txt"` still counts and `dir + "/"` does not.
              let separators =
                  let total = List.length operands

                  operands
                  |> List.indexed
                  |> List.choose (fun (i, o) ->
                      match o with
                      | SynExpr.Const(SynConst.String(text, SynStringKind.Regular, _), _) ->
                          separatorOf text
                          |> Option.filter (fun sep ->
                              let sepChar = sep.[0]

                              if i > 0 && i < total - 1 then true
                              elif i = total - 1 then text.TrimEnd sepChar <> ""
                              else text.TrimStart sepChar <> "")
                      | _ -> None)
                  |> List.distinct

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

              // BOTH separators need positive evidence. A lone backslash
              // used to read as path-ish on its own, but a corpus run over
              // FsAutoComplete showed where that goes wrong: escape-sequence
              // building (`result <- result + "\\" + string c`) is full of
              // backslash literals and has nothing to do with paths.
              // Evidence is path-flavored names, a rooted or
              // extension-bearing literal, or a literal existing on disk.
              // evidence that this is a FILESYSTEM path, not just something
              // path-shaped: a rooted or extension-bearing literal, or one
              // that actually exists on this machine
              let hasStrongEvidence =
                  literalTexts
                  |> List.exists (fun t -> rootedLiteral.IsMatch t || extensionLiteral.IsMatch t || existsOnDisk t)

              // A chain opening with a forward-slash literal is as likely a
              // URL path as a filesystem one — `"/img/userimages/" + fileId`
              // is a web route, and Path.Combine would turn it into
              // backslashes. A path-flavored NAME is too weak to tell those
              // apart (`fileId` matches "file"), so the leading-slash case
              // wants the stronger evidence.
              let opensWithSlashLiteral =
                  match operands with
                  | SynExpr.Const(SynConst.String(text, SynStringKind.Regular, _), _) :: _ -> text.StartsWith '/'
                  | _ -> false

              let hasPathEvidence (_separator: string) =
                  if opensWithSlashLiteral then
                      hasStrongEvidence
                  else
                      pathSmell.IsMatch(textOfRange source expr.Range) || hasStrongEvidence

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
