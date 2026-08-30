/// Refactoring: pull a non-recursive member out of a `let rec ... and`
/// group (FR0116, fix).
///
///     let rec f1 x = f3 x + x         let f2 y = y + 1
///     and f2 y = y + 1           →
///     and f3 z = f1 z - z             let rec f1 x = f3 x + x
///                                     and f3 z = f1 z - z
///
/// A binding that references no member of its group takes part in no
/// recursion; carrying it in the `and` chain only widens the knot. It
/// moves to a plain `let` ABOVE the group — members may call it from
/// there, and it can call nothing in the group by construction.
///
/// A self-recursive member (calls itself, nobody else) leaves the same
/// way but keeps its own `let rec`. And when the HEAD is the one that
/// references nobody, nothing moves at all: `findHeadRecrowns` turns its
/// `let rec` into `let` and re-crowns the next binding — see below.
///
/// Safety rules (v1 keeps the surgery small):
///   - module-level groups only; the head is handled by re-crowning, the
///     rest by moving out
///   - the binding may carry no attributes, and nothing but whitespace may
///     sit between the previous binding's end and its `and` (a comment
///     there would be orphaned — and the comment guard would hold the fix
///     back anyway)
///   - group-membership is judged textually: any `\b<member>\b` inside the
///     binding block keeps it in the group, so a shadowed name errs toward
///     staying put
module FSharp.Refactor.RecGroup

open System.Text.RegularExpressions
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        /// The `and` binding's whole block — replaced with nothing.
        RemoveRange: range
        /// Zero-width, at the group's start — the plain `let` goes here.
        InsertRange: range
        InsertText: string
        MemberName: string
        /// The member calls itself (but nobody else): it leaves the group
        /// as its own `let rec`, not a plain `let`.
        IsSelfRecursive: bool
    }

/// The HEAD of a group references no sibling: nothing needs to move at
/// all — the head's `let rec` becomes `let`, and the next binding's
/// `and` is re-crowned `let rec`. Two keyword rewrites, in place.
type HeadSuggestion =
    {
        /// The head's `let rec` keyword — becomes `let`.
        LetRecRange: range
        /// The second binding's `and` keyword — becomes `let rec`.
        AndRange: range
        MemberName: string
    }

let private bindingName (SynBinding(headPat = pat)) =
    match pat with
    | SynPat.Named(ident = SynIdent(ident = id)) -> Some id.idText
    | SynPat.LongIdent(longDotId = SynLongIdent(id = [ id ]); argPats = SynArgPats.Pats _) -> Some id.idText
    | _ -> None

let rec private declsOf (decls: SynModuleDecl list) : SynModuleDecl list =
    decls
    |> List.collect (fun d ->
        match d with
        | SynModuleDecl.NestedModule(decls = nested) -> d :: declsOf nested
        | _ -> [ d ])

let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let decls =
        match parseTree with
        | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
            modules
            |> List.collect (fun (SynModuleOrNamespace(decls = ds)) -> declsOf ds)
        | _ -> []

    [ for decl in decls do
          match decl with
          | SynModuleDecl.Let(isRecursive = true; bindings = bindings) when bindings.Length >= 2 ->
              let names = bindings |> List.choose bindingName

              // every binding must have a recognizable name, or membership
              // is unknowable
              if names.Length = bindings.Length then
                  // each binding's text block runs from its leading keyword
                  // to the next binding's leading keyword (or the group end)
                  let keywordStarts =
                      bindings
                      |> List.map (fun (SynBinding(trivia = trivia)) -> trivia.LeadingKeyword.Range)

                  let blockOf i =
                      let start = (List.item i keywordStarts).Start

                      let finish =
                          if i = bindings.Length - 1 then
                              decl.Range.End
                          else
                              (List.item (i + 1) keywordStarts).Start

                      Range.mkRange decl.Range.FileName start finish

                  for i in 1 .. bindings.Length - 1 do
                      let keywordLine = (List.item i keywordStarts).StartLine
                      let name = List.item i names

                      // comment lines directly above the `and` belong to the
                      // member when they are doc comments (///) or mention
                      // its name; those travel with it. An unrelated comment
                      // stays put — and blocks nothing
                      let commentStartLine =
                          let mutable first = keywordLine

                          let mutable scanning = true

                          while scanning && first > 1 do
                              let above = source.GetLineString(first - 2).Trim()

                              if
                                  above.StartsWith "///"
                                  || (above.StartsWith "//"
                                      && Regex.IsMatch(above, identifierPattern name))
                              then
                                  first <- first - 1
                              else
                                  scanning <- false

                          first

                      let plainBlock = blockOf i
                      let bindingText = textOfRange source plainBlock

                      let companionComments =
                          [ for l in commentStartLine .. keywordLine - 1 -> source.GetLineString(l - 1) ]

                      let block =
                          if commentStartLine < keywordLine then
                              Range.mkRange
                                  plainBlock.FileName
                                  (Position.mkPos commentStartLine 0)
                                  plainBlock.End
                          else
                              plainBlock

                      let (SynBinding(attributes = attrs)) = List.item i bindings

                      // membership judged on the BINDING text: a companion
                      // comment naming a sibling should not keep it in
                      let referencesGroup =
                          names
                          |> List.exists (fun other ->
                              other <> name && Regex.IsMatch(bindingText, identifierPattern other))

                      // its own name beyond the header means self-recursion:
                      // the member still leaves, but as its own `let rec` —
                      // a plain `let` would not compile
                      let isSelfRecursive =
                          Regex.Matches(bindingText, identifierPattern name).Count >= 2

                      if
                          attrs.IsEmpty
                          && not referencesGroup
                          && not (spansDirective source block)
                          // the binding must start with its `and`, on its own
                          // line — mid-line groups are not worth the surgery
                          && bindingText.StartsWith "and"
                          && (source.GetLineString(plainBlock.StartLine - 1))
                              .Substring(0, plainBlock.StartColumn)
                              .Trim() = ""
                      then
                          let indent = String.replicate decl.Range.StartColumn " "

                          // `and f2 y = ...` becomes `let f2 y = ...`, its
                          // companion comments riding along above it
                          let commentPrefix =
                              match companionComments with
                              | [] -> ""
                              | lines -> (lines |> String.concat "\n") + $"\n{indent}"

                          let keyword = if isSelfRecursive then "let rec" else "let"
                          let extracted = commentPrefix + keyword + bindingText.Substring(3)

                          { RemoveRange = block
                            InsertRange = Range.mkRange decl.Range.FileName decl.Range.Start decl.Range.Start
                            InsertText = extracted.TrimEnd() + $"\n\n{indent}"
                            MemberName = name
                            IsSelfRecursive = isSelfRecursive }
          | _ -> () ]

let private letRecKeywordRegex = Regex @"^let\s+rec$"
/// Head extraction: the FIRST binding of the group references no member
/// (itself included). Unlike an `and` extraction nothing moves — the
/// head already sits above the rest — so the fix is two keyword
/// rewrites: `let rec` → `let` on the head, `and` → `let rec` on the
/// next binding. Comments and attributes never enter into it.
let findHeadRecrowns (parseTree: ParsedInput) (source: ISourceText) : HeadSuggestion list =
    // an `and` extraction in the same group would overlap these keyword
    // edits; let it go first — the multi-pass loop revisits the group
    let takenGroups =
        find parseTree source |> List.map (fun s -> s.RemoveRange.StartLine)

    let decls =
        match parseTree with
        | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
            modules
            |> List.collect (fun (SynModuleOrNamespace(decls = ds)) -> declsOf ds)
        | _ -> []

    [ for decl in decls do
          match decl with
          | SynModuleDecl.Let(isRecursive = true; bindings = bindings) when bindings.Length >= 2 ->
              let names = bindings |> List.choose bindingName

              let groupHasExtraction =
                  takenGroups
                  |> List.exists (fun line -> line >= decl.Range.StartLine && line <= decl.Range.EndLine)

              if names.Length = bindings.Length && not groupHasExtraction then
                  let keywordRanges =
                      bindings
                      |> List.map (fun (SynBinding(trivia = trivia)) -> trivia.LeadingKeyword.Range)

                  let headKeyword = List.head keywordRanges
                  let andKeyword = List.item 1 keywordRanges
                  let headName = List.head names

                  // the head's text block: its keyword to the next keyword
                  let headText =
                      textOfRange source (Range.mkRange decl.Range.FileName headKeyword.Start andKeyword.Start)

                  let referencesAnyMember =
                      // itself included: a self-recursive head must keep its
                      // `rec`, and that variant is not worth the surgery
                      names
                      |> List.exists (fun other ->
                          let occurrencesBeyondHeader = if other = headName then 1 else 0

                          Regex.Matches(headText, identifierPattern other).Count > occurrencesBeyondHeader)

                  let startsOwnLine (r: range) =
                      (source.GetLineString(r.StartLine - 1)).Substring(0, r.StartColumn).Trim() = ""

                  if
                      not referencesAnyMember
                      && isSingleLine headKeyword
                      && letRecKeywordRegex.IsMatch (textOfRange source headKeyword)
                      && textOfRange source andKeyword = "and"
                      && startsOwnLine headKeyword
                      && startsOwnLine andKeyword
                      && not (spansDirective source decl.Range)
                  then
                      { LetRecRange = headKeyword
                        AndRange = andKeyword
                        MemberName = headName }
          | _ -> () ]
