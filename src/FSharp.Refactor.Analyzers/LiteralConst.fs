/// FR0130 (fix, OFF by default): a module-level constant binding gains
/// [<Literal>]:
///
///     let ConnectionName = "orders"      [<Literal>]
///                                    →   let ConnectionName = "orders"
///
/// A literal can be used in patterns and attribute arguments and is
/// const-folded at use sites. OFF by default because blanket-annotating
/// every constant is churn — turn it on per repository when magic-string
/// hygiene is the goal. Contained (private/internal) bindings only
/// unless --api-changes: [<Literal>] compiles a public field to a CONST,
/// which is a binary-compatibility change.
module FSharp.Refactor.LiteralConst

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type Suggestion =
    {
        Range: range
        Name: string
        /// Zero-width insert point (line start of the `let`) and the
        /// attribute line.
        Fix: range * string
    }

let private isConstant (e: SynExpr) =
    match e with
    | SynExpr.Const(c, _) ->
        match c with
        | SynConst.String(_, SynStringKind.Regular, _)
        | SynConst.String(_, SynStringKind.Verbatim, _)
        | SynConst.Int32 _
        | SynConst.Int64 _
        | SynConst.Byte _
        | SynConst.UInt32 _
        | SynConst.UInt64 _
        | SynConst.Char _
        | SynConst.Bool _
        | SynConst.Double _
        | SynConst.Single _ -> true
        | _ -> false
    | _ -> false

/// A simple value binder and its own access modifier — `let private X`
/// parks the modifier on the pattern, and an identifier can parse as
/// Named or a no-argument LongIdent.
let private valueBinder (p: SynPat) =
    match p with
    | SynPat.Named(ident = SynIdent(ident = id); accessibility = acc) -> ValueSome(id, acc)
    | SynPat.LongIdent(longDotId = SynLongIdent(id = [ id ]); argPats = SynArgPats.Pats []; accessibility = acc) ->
        ValueSome(id, acc)
    | _ -> ValueNone

let find (allowApiChanges: bool) (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    // Names that appear as a bare identifier PATTERN anywhere in the file.
    // `match s with | greeting -> ...` binds today; once `greeting` carries
    // [<Literal>] the same pattern MATCHES THE CONSTANT — it compiles, and
    // the behavior silently changes. Local `let greeting = ...` binders are
    // patterns too and would turn into partial matches. Any same-named bare
    // pattern (either parse shape a lone identifier takes) other than the
    // candidate's own binder vetoes the annotation.
    let patternIdents =
        [ for _, p in index.Pats do
              match p with
              | SynPat.Named(ident = SynIdent(ident = id))
              | SynPat.LongIdent(longDotId = SynLongIdent(id = [ id ]); argPats = SynArgPats.Pats []) ->
                  id.idText, id.idRange
              | _ -> () ]

    let vetoed (id: Ident) =
        patternIdents
        |> List.exists (fun (text, r) -> text = id.idText && not (Range.equals r id.idRange))

    [ for path, decl in index.Decls do
          match decl with
          | SynModuleDecl.Let(isRecursive = false; bindings = [ binding ]) ->
              match binding with
              | SynBinding(
                  accessibility = access
                  attributes = []
                  isMutable = false
                  isInline = false
                  headPat = pat
                  expr = rhs
                  trivia = trivia) when isConstant rhs ->
                  match valueBinder pat with
                  | ValueSome(id, patAccess) when
                      Visibility.isInScope allowApiChanges path [ access; patAccess ]
                      && not (vetoed id)
                      ->
                      let kw = trivia.LeadingKeyword.Range

                      let ownLine =
                          kw.StartColumn = 0
                          || (source.GetLineString(kw.StartLine - 1)).Substring(0, kw.StartColumn).Trim() = ""

                      if ownLine then
                          let indent = String.replicate kw.StartColumn " "
                          let at = Position.mkPos kw.StartLine 0

                          { Range = id.idRange
                            Name = id.idText
                            Fix = Range.mkRange decl.Range.FileName at at, $"{indent}[<Literal>]\n" }
                  | _ -> ()
              | _ -> ()
          | _ -> () ]
