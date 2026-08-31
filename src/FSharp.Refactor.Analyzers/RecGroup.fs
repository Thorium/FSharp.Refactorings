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

/// The identifiers a binding is USED by. An active pattern's definition
/// name is `|SqlColumnGet|_|`, but every use site says `SqlColumnGet` —
/// checking the decorated name finds nothing, which read as "references
/// no sibling" on SQLProvider's mutually recursive pattern grammar and
/// offered extractions that could not hold together.
let private referenceNames (name: string) =
    if name.Contains '|' then
        name.Split '|'
        |> Array.filter (fun part -> part <> "" && part <> "_")
        |> List.ofArray
    else
        [ name ]

/// Any use of `name` (by any of its reference identifiers) in the text.
let private mentions (text: string) (name: string) =
    referenceNames name
    |> List.exists (fun part -> Regex.IsMatch(text, identifierPattern part))

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
            modules |> List.collect (fun (SynModuleOrNamespace(decls = ds)) -> declsOf ds)
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

                  let suggestionFor i =
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

                              if above.StartsWith "///" || (above.StartsWith "//" && mentions above name) then
                                  first <- first - 1
                              else
                                  scanning <- false

                          first

                      let plainBlock = blockOf i
                      let bindingText = textOfRange source plainBlock

                      let companionComments =
                          [ for l in commentStartLine .. keywordLine - 1 -> source.GetLineString(l - 1) ]

                      // an INDENTED group's plain block runs keyword-to-
                      // keyword: removing it is column-symmetric and the
                      // next binding keeps its indentation. The comment-
                      // extended block starts at column 0, so it must END
                      // at column 0 too — ending at the next keyword's
                      // column ate the following `and`'s indent inside
                      // nested modules (VQC.fs, FSharp.Azure.Quantum) and
                      // left it orphaned at the margin
                      let block =
                          if commentStartLine < keywordLine then
                              let endPos =
                                  if i < bindings.Length - 1 && plainBlock.End.Column > 0 then
                                      Position.mkPos plainBlock.EndLine 0
                                  else
                                      plainBlock.End

                              Range.mkRange plainBlock.FileName (Position.mkPos commentStartLine 0) endPos
                          else
                              plainBlock

                      let (SynBinding(attributes = attrs)) = List.item i bindings

                      // membership judged on the BINDING text: a companion
                      // comment naming a sibling should not keep it in
                      let referencesGroup =
                          names |> List.exists (fun other -> other <> name && mentions bindingText other)

                      // its own name beyond the header means self-recursion:
                      // the member still leaves, but as its own `let rec` —
                      // a plain `let` would not compile
                      let isSelfRecursive =
                          referenceNames name
                          |> List.exists (fun part -> Regex.Matches(bindingText, identifierPattern part).Count >= 2)

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

                          Some
                              { RemoveRange = block
                                InsertRange = Range.mkRange decl.Range.FileName decl.Range.Start decl.Range.Start
                                // the insert point sits AFTER the group's
                                // existing indentation: the first inserted
                                // line must not bring its own (raw comment
                                // lines carry it; inside a nested module
                                // that doubled up)
                                InsertText = extracted.TrimStart().TrimEnd() + $"\n\n{indent}"
                                MemberName = name
                                IsSelfRecursive = isSelfRecursive }
                      else
                          None

                  // ONE suggestion per group per pass: several would all
                  // insert at the group's start, and every message after
                  // the first would only be held back as un-appliable —
                  // the multi-pass loop revisits for the rest
                  match [ 1 .. bindings.Length - 1 ] |> List.tryPick suggestionFor with
                  | Some s -> s
                  | None -> ()
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
            modules |> List.collect (fun (SynModuleOrNamespace(decls = ds)) -> declsOf ds)
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
                      // `rec`, and that variant is not worth the surgery.
                      // Names are checked by their USE identifiers — an
                      // active pattern is used by its case names, not its
                      // decorated `|A|_|` definition name
                      names
                      |> List.exists (fun other ->
                          let occurrencesBeyondHeader = if other = headName then 1 else 0

                          referenceNames other
                          |> List.exists (fun part ->
                              Regex.Matches(headText, identifierPattern part).Count > occurrencesBeyondHeader))

                  let startsOwnLine (r: range) =
                      (source.GetLineString(r.StartLine - 1)).Substring(0, r.StartColumn).Trim() = ""

                  if
                      not referencesAnyMember
                      && isSingleLine headKeyword
                      && letRecKeywordRegex.IsMatch(textOfRange source headKeyword)
                      && textOfRange source andKeyword = "and"
                      && startsOwnLine headKeyword
                      && startsOwnLine andKeyword
                      && not (spansDirective source decl.Range)
                  then
                      { LetRecRange = headKeyword
                        AndRange = andKeyword
                        MemberName = headName }
          | _ -> () ]
