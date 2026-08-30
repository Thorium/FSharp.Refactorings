/// Refactoring: complete an incomplete DU match by adding the missing
/// cases as explicit arms (FR0110, fix).
///
///     match color with            match color with
///     | Red -> "r"           →    | Red -> "r"
///     | Green -> "g"              | Green -> "g"
///                                 | Blue -> raise (System.NotImplementedException())
///
/// The dual of FR0072: that rule expands a wildcard hiding one or two real
/// cases, this one closes a match that has no wildcard at all — the FS0025
/// warning shape. The added arm raises, FR0100-style, so the gap reports
/// itself instead of silently returning something plausible.
///
/// Safety rules:
///   - every clause must cover its cases TOTALLY (bare case names, fields
///     as wildcards, or-patterns fine); any literal, guard-only case use,
///     variable or partial pattern makes coverage unknowable — skipped
///   - clauses carrying `when` guards do not count as covering their case
///     (the guard may reject), but their presence is fine
///   - the scrutinee's type must resolve to an F# union, all covered names
///     must belong to it, and at most three cases may be missing — past
///     that, a wildcard arm was probably the intent
///   - multi-line matches only: each clause starts its own line, and the
///     new arms adopt the last clause's `|` column
module FSharp.Refactor.MissingCases

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        /// Zero-width range at the end of the last clause — the arms are
        /// INSERTED, nothing is replaced.
        Range: range
        InsertText: string
        MissingCases: string list
    }

/// A pattern that always matches whatever it is applied to.
[<TailCall>]
let rec private isTotalLoop (pending: SynPat list) : bool =
    match pending with
    | [] -> true
    | p :: rest ->
        match p with
        | SynPat.Wild _
        | SynPat.Named _ -> isTotalLoop rest
        | SynPat.Paren(inner, _)
        | SynPat.Typed(pat = inner)
        | SynPat.Attrib(pat = inner) -> isTotalLoop (inner :: rest)
        | SynPat.Tuple(elementPats = ps) -> isTotalLoop (ps @ rest)
        | _ -> false

/// The case idents a clause pattern covers totally, or None when the
/// pattern is anything but plain total case coverage.
[<TailCall>]
let rec private coveredLoop (acc: Ident list list) (pending: SynPat list) : Ident list list option =
    match pending with
    | [] -> Some(List.rev acc)
    | p :: rest ->
        match p with
        | SynPat.LongIdent(longDotId = SynLongIdent(id = ids); argPats = SynArgPats.Pats args) when
            not ids.IsEmpty && isTotalLoop args
            ->
            coveredLoop (ids :: acc) rest
        | SynPat.Paren(inner, _) -> coveredLoop acc (inner :: rest)
        | SynPat.Or(lhsPat = l; rhsPat = r) -> coveredLoop acc (l :: r :: rest)
        | _ -> None

let private unionCasesOf (check: FSharpCheckFileResults) (source: ISourceText) (caseIdent: Ident) =
    let r = caseIdent.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ caseIdent.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpUnionCase as unionCase ->
            try
                let t = OptionModule.stripAbbreviations unionCase.ReturnType

                if t.HasTypeDefinition && t.TypeDefinition.IsFSharpUnion then
                    Some [ for c in t.TypeDefinition.UnionCases -> c.Name, c.Fields.Count > 0 ]
                else
                    None
            with OptionModule.FcsSymbolFailure ->
                None
        | _ -> None
    | None -> None

/// Find incomplete DU matches with no catch-all. Requires typed check
/// results for the union lookup.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.Match(clauses = clauses)
              | SynExpr.MatchBang(clauses = clauses) when not clauses.IsEmpty ->
                  // any total (wildcard/variable) clause completes the match
                  let anyCatchAll =
                      clauses
                      |> List.exists (fun (SynMatchClause(pat = p)) ->
                          match p with
                          | SynPat.Wild _
                          | SynPat.Named _ -> true
                          | _ -> false)

                  let unguarded =
                      clauses |> List.filter (fun (SynMatchClause(whenExpr = g)) -> g.IsNone)

                  let covered =
                      unguarded |> List.map (fun (SynMatchClause(pat = p)) -> coveredLoop [] [ p ])

                  // guarded clauses must still be PLAIN case patterns, or
                  // coverage of the whole match is beyond this rule
                  let guardedParseable =
                      clauses
                      |> List.forall (fun (SynMatchClause(pat = p; whenExpr = g)) ->
                          g.IsNone || (coveredLoop [] [ p ]) |> Option.isSome)

                  if
                      not anyCatchAll
                      && guardedParseable
                      && not covered.IsEmpty
                      && covered |> List.forall Option.isSome
                  then
                      let coveredIdents = covered |> List.choose id |> List.concat

                      match coveredIdents with
                      | first :: _ ->
                          match unionCasesOf check source (List.last first) with
                          | Some allCases ->
                              let coveredNames =
                                  coveredIdents |> List.map (fun ids -> (List.last ids).idText) |> Set.ofList

                              let missing =
                                  allCases |> List.filter (fun (name, _) -> not (coveredNames.Contains name))

                              let qualifier =
                                  first
                                  |> List.rev
                                  |> List.tail
                                  |> List.rev
                                  |> List.map (fun i -> i.idText + ".")
                                  |> String.concat ""

                              let lastClause = List.last clauses
                              let lastLine = source.GetLineString(lastClause.Range.EndLine - 1)
                              let firstLine = source.GetLineString(lastClause.Range.StartLine - 1)
                              let barColumn = firstLine.IndexOf '|'

                              // every covered name must belong to THIS union
                              // (same-named cases of another DU would slip
                              // through the name-set comparison otherwise)
                              if
                                  coveredNames
                                  |> Set.forall (fun n -> allCases |> List.exists (fun (c, _) -> c = n))
                                  && not missing.IsEmpty
                                  && missing.Length <= 3
                                  // multi-line matches only, clause starting
                                  // its line at a findable bar
                                  && barColumn >= 0
                                  && firstLine.Substring(0, barColumn).Trim() = ""
                                  // nothing trails the last clause on its
                                  // final line
                                  && lastLine.Length <= lastClause.Range.EndColumn
                              then
                                  let indent = String.replicate barColumn " "

                                  let insertText =
                                      missing
                                      |> List.map (fun (name, hasFields) ->
                                          let pattern =
                                              if hasFields then
                                                  $"{qualifier}{name} _"
                                              else
                                                  $"{qualifier}{name}"

                                          $"\n{indent}| {pattern} -> raise (System.NotImplementedException())")
                                      |> String.concat ""

                                  let insertAt =
                                      Range.mkRange lastClause.Range.FileName lastClause.Range.End lastClause.Range.End

                                  { Range = insertAt
                                    InsertText = insertText
                                    MissingCases = missing |> List.map fst }
                          | None -> ()
                      | [] -> ()
              | _ -> () ]
