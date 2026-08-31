/// Refactoring (slide 3, single-file variant): give the unnamed tuple fields
/// of a non-public union case the names the code already spells — from the
/// strongest source that yields them:
///
///   1. its MATCH SITES:      | Line(qty, price) -> ..
///   2. its TRAILING COMMENT: | Line of int * decimal // qty and price
///                            (also `// qty * price` and `// qty, price`)
///   3. its OWN NAME:         | QtyAndPrice of int * decimal
///
///     type private Order =                type private Order =
///         | Line of int * decimal    →        | Line of qty: int * price: decimal
///     ...
///     match o with
///     | Line(qty, price) -> ...           (match sites are positional and
///                                          stay valid unchanged)
///
/// Safety rules:
///   - the field names must be invisible outside the assembly, so no
///     serializer or reflection consumer in another assembly can observe
///     the compiled name change: the type or its representation is private
///     or internal, or the type sits in a private/internal module
///   - the case has >= 2 fields, all currently unnamed
///   - every destructuring site binds every position with a plain lowercase
///     name and all sites agree on the names; `Case _` sites are fine, any
///     other shape disqualifies the case
///   - the case name is unique among the file's union cases, so pattern
///     sites attribute unambiguously
///   - only the definition is edited; construction and matching are
///     positional and stay valid, in this file and in later files
module FSharp.Refactor.DuFieldNames

open System
open System.Collections.Generic
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        CaseName: string
        /// Field names in field order, from the winning source.
        Names: string list
        /// Where the names came from, for the message: "its match sites",
        /// "its trailing comment" or "its own name".
        Source: string
        /// The union case definition, for the hint location.
        Range: range
        /// One insertion per field ((range, original, replacement)).
        Edits: (range * string * string) list
    }

/// Words that spell TYPES or syntax, not field names — `// string * int`
/// is a type note, and a keyword would not compile as a field name.
let private notFieldNames =
    set
        [ "string"
          "int"
          "int8"
          "int16"
          "int32"
          "int64"
          "uint"
          "uint8"
          "uint16"
          "uint32"
          "uint64"
          "byte"
          "sbyte"
          "float"
          "float32"
          "double"
          "single"
          "decimal"
          "bool"
          "char"
          "unit"
          "obj"
          "option"
          "voption"
          "list"
          "array"
          "seq"
          "nativeint"
          "unativeint"
          "let"
          "type"
          "of"
          "if"
          "then"
          "else"
          "match"
          "with"
          "fun"
          "function"
          "when"
          "true"
          "false"
          "null"
          "begin"
          "end"
          "module"
          "member"
          "use"
          "do"
          "done"
          "rec"
          "in"
          "and"
          "or"
          "not"
          "to"
          "val"
          "open"
          "base"
          "default"
          "delegate"
          "interface"
          "inherit"
          "lazy"
          "return"
          "yield"
          "mutable"
          "internal"
          "private"
          "public"
          "static"
          "override"
          "abstract"
          "new"
          "try"
          "finally"
          "while"
          "for"
          "as"
          "assert"
          "class"
          "struct"
          "exception"
          "extern"
          "fixed"
          "global"
          "namespace"
          "elif"
          "downcast"
          "upcast" ]

/// Names from the case's own name: `InterestAndRate` -> [interest; rate],
/// `StartDateAndEndDate` -> [startDate; endDate]. The `And` must sit at a
/// camel boundary, so `Command` never splits — and a part that lowers to
/// a keyword (`BeginAndEnd`) would not compile as a field name.
let namesFromCaseName (caseName: string) (arity: int) : string list option =
    let parts =
        Text.RegularExpressions.Regex.Split(caseName, @"(?<=[a-z0-9])And(?=[A-Z])")

    if
        parts.Length = arity
        && parts
           |> Array.forall (fun p -> p.Length > 0 && Char.IsUpper p.[0] && not (p.Contains '_'))
    then
        let lowered =
            parts
            |> Array.map (fun p -> string (Char.ToLowerInvariant p.[0]) + p.Substring 1)
            |> List.ofArray

        if
            (lowered |> List.distinct |> List.length) = arity
            && lowered |> List.forall (notFieldNames.Contains >> not)
        then
            Some lowered
        else
            None
    else
        None

/// Names from a trailing same-line comment with a clear list format:
/// `// interest and rate`, `// interest * rate`, `// interest, rate`.
let namesFromComment (commentText: string) (arity: int) : string list option =
    let body = (commentText.TrimStart '/').Trim()

    let parts =
        Text.RegularExpressions.Regex.Split(body, @"\s+and\s+|\s*\*\s*|\s*,\s*")
        |> Array.filter (fun s -> s <> "")

    if
        parts.Length = arity
        && parts
           |> Array.forall (fun p ->
               Text.RegularExpressions.Regex.IsMatch(p, @"^[a-z][A-Za-z0-9_]*$")
               && not (notFieldNames.Contains p))
    then
        let names = List.ofArray parts

        if (names |> List.distinct |> List.length) = arity then
            Some names
        else
            None
    else
        None

/// What one destructuring site tells us about a case's field names.
[<RequireQualifiedAccess>]
type private Observation =
    /// A full tuple pattern binding every position to a plain name.
    | Names of string list
    /// `Case _` — no information, but no conflict either.
    | Opaque
    /// Any other destructuring shape; disqualifies the case.
    | Bad

/// Record every union-case-looking pattern application in the pending
/// pattern trees.
[<TailCall>]
let rec private walkPatsLoop (record: string -> Observation -> unit) (pending: SynPat list) =
    match pending with
    | [] -> ()
    | pat :: rest ->
        let next =
            match pat with
            | SynPat.LongIdent(longDotId = SynLongIdent(id = ids); argPats = SynArgPats.Pats pats) ->
                let name = (List.last ids).idText

                (match pats with
                 | [] -> ()
                 | [ SynPat.Paren(SynPat.Tuple(elementPats = elems), _) ] ->
                     let names =
                         elems
                         |> List.map (fun element ->
                             match element with
                             | SynPat.Named(ident = SynIdent(ident = id)) -> Some id.idText
                             | _ -> None)

                     if names |> List.forall Option.isSome then
                         record name (Observation.Names(names |> List.choose id))
                     else
                         record name Observation.Bad
                 | [ SynPat.Wild _ ] -> record name Observation.Opaque
                 | _ -> record name Observation.Bad)

                pats @ rest
            | SynPat.Paren(p, _)
            | SynPat.Typed(pat = p)
            | SynPat.Attrib(pat = p) -> p :: rest
            | SynPat.As(lhsPat = l; rhsPat = r)
            | SynPat.Or(lhsPat = l; rhsPat = r)
            | SynPat.ListCons(lhsPat = l; rhsPat = r) -> l :: r :: rest
            | SynPat.Ands(pats = ps)
            | SynPat.Tuple(elementPats = ps)
            | SynPat.ArrayOrList(elementPats = ps) -> ps @ rest
            | SynPat.Record(fieldPats = fieldPats) ->
                (fieldPats |> List.map (fun (NamePatPairField(pat = p)) -> p)) @ rest
            | _ -> rest

        walkPatsLoop record next

let private walkPat record pat = walkPatsLoop record [ pat ]

/// Find non-public union cases whose unnamed fields can take the names their
/// match sites already bind. Under `--api-changes` public unions qualify
/// too; the names then come from whatever destructuring sites this file
/// happens to hold, which is partial evidence but never an unsafe edit —
/// naming fields leaves positional patterns working everywhere.
let find (allowApiChanges: bool) (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    // every union case name in the file, to detect ambiguous attribution
    let caseNameCounts = Dictionary<string, int>()
    // (case name, unnamed fields, case definition range)
    let candidates = ResizeArray<string * SynField list * range>()

    for path, decl in index.Decls do
        match decl with
        | SynModuleDecl.Types(typeDefns = defns) ->
            for SynTypeDefn(typeInfo = SynComponentInfo(accessibility = typeAcc); typeRepr = repr) in defns do
                match repr with
                | SynTypeDefnRepr.Simple(
                    simpleRepr = SynTypeDefnSimpleRepr.Union(accessibility = reprAcc; unionCases = cases)) ->
                    let confined = Visibility.isInScope allowApiChanges path [ typeAcc; reprAcc ]

                    for SynUnionCase(ident = SynIdent(ident = caseId); caseType = kind; range = caseRange) in cases do
                        caseNameCounts.[caseId.idText] <-
                            (match caseNameCounts.TryGetValue caseId.idText with
                             | true, n -> n
                             | _ -> 0)
                            + 1

                        match kind with
                        | SynUnionCaseKind.Fields fields when
                            confined
                            && fields.Length >= 2
                            && fields |> List.forall (fun (SynField(idOpt = idOpt)) -> idOpt.IsNone)
                            ->
                            candidates.Add(caseId.idText, fields, caseRange)
                        | _ -> ()
                | _ -> ()
        | _ -> ()

    if candidates.Count = 0 then
        []
    else
        // harvest naming observations from every pattern position in the file
        let observations = Dictionary<string, ResizeArray<Observation>>()

        let record name obs =
            match observations.TryGetValue name with
            | true, existing -> existing.Add obs
            | _ ->
                let fresh = ResizeArray()
                fresh.Add obs
                observations.[name] <- fresh

        for _, decl in index.Decls do
            match decl with
            | SynModuleDecl.Let(bindings = bindings) ->
                for SynBinding(headPat = p) in bindings do
                    walkPat record p
            | _ -> ()

        for _, expr in index.Exprs do
            match expr with
            | SynExpr.Match(clauses = clauses)
            | SynExpr.MatchBang(clauses = clauses)
            | SynExpr.MatchLambda(matchClauses = clauses)
            | SynExpr.TryWith(withCases = clauses) ->
                for SynMatchClause(pat = p) in clauses do
                    walkPat record p
            | SynExpr.Lambda(parsedData = Some(pats, _)) -> pats |> List.iter (walkPat record)
            | SynExpr.LetOrUse lou ->
                for SynBinding(headPat = p) in lou.Bindings do
                    walkPat record p
            | SynExpr.ForEach(pat = p) -> walkPat record p
            | _ -> ()

        // trailing line comments, for the comment name source
        let comments =
            lazy
                (commentsWithText parseTree source
                 |> List.filter (fun (_, t) -> t.StartsWith "//" && not (t.StartsWith "///")))

        [ for caseName, fields, caseRange in candidates do
              let sites =
                  match observations.TryGetValue caseName with
                  | true, l -> List.ofSeq l
                  | _ -> []

              let tupleSites =
                  sites
                  |> List.choose (function
                      | Observation.Names names -> Some names
                      | Observation.Opaque
                      | Observation.Bad -> None)

              let siteNames =
                  if
                      caseNameCounts.[caseName] = 1
                      && not (sites |> List.exists ((=) Observation.Bad))
                      && not tupleSites.IsEmpty
                      && tupleSites |> List.forall ((=) tupleSites.Head)
                      && tupleSites.Head.Length = fields.Length
                      && (tupleSites.Head |> List.distinct |> List.length) = fields.Length
                      && tupleSites.Head |> List.forall (fun n -> n.Length > 0 && Char.IsLower n.[0])
                  then
                      Some(tupleSites.Head, "its match sites")
                  else
                      None

              // weaker sources only speak when the sites are silent — a
              // definition-only edit is safe either way, so site quality
              // never blocks them
              let commentNames =
                  comments.Value
                  |> List.tryPick (fun (cr, ctext) ->
                      let lineText = source.GetLineString(cr.StartLine - 1)

                      if
                          cr.StartLine = caseRange.EndLine
                          && cr.StartColumn >= caseRange.EndColumn
                          && lineText.Substring(cr.EndColumn).Trim() = ""
                          // one case per line: with `| A of .. | B of .. // names`
                          // the comment cannot say WHICH case it describes
                          && (lineText.Substring(0, cr.StartColumn) |> Seq.filter ((=) '|') |> Seq.length)
                             <= 1
                      then
                          namesFromComment ctext fields.Length
                      else
                          None)
                  |> Option.map (fun n -> n, "its trailing comment")

              let named =
                  siteNames
                  |> Option.orElse commentNames
                  |> Option.orElse (
                      namesFromCaseName caseName fields.Length
                      |> Option.map (fun n -> n, "its own name")
                  )

              match named with
              | Some(names, sourceName) ->
                  let edits =
                      // the inserted names take the tuple's OWN spacing:
                      // `int * int` gains `rx: int * ry: int`, the compact
                      // `int*int` gains `rx:int*ry:int` — a space after the
                      // colon in a spaceless tuple reads lopsided
                      let fieldsText =
                          match fields with
                          | first :: _ ->
                              let (SynField(range = fr)) = first
                              let (SynField(range = lr)) = List.last fields
                              textOfRange source (Range.mkRange fr.FileName fr.Start lr.End)
                          | [] -> ""

                      let colon =
                          if fieldsText.Contains " * " || fields.Length = 1 then
                              ": "
                          else
                              ":"

                      List.zip names fields
                      |> List.map (fun (name, SynField(range = fieldRange)) ->
                          let original = textOfRange source fieldRange
                          fieldRange, original, $"{name}{colon}{original}")

                  { CaseName = caseName
                    Names = names
                    Source = sourceName
                    Range = caseRange
                    Edits = edits }
              | None -> () ]
