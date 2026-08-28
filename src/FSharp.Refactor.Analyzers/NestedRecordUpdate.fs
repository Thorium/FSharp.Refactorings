/// Refactoring (modernization, F# 8): updating a nested record field no
/// longer needs a copy-and-update per level.
///
///     { r with X = { r.X with Y = v } }        →  { r with X.Y = v }
///     { r with X = { r.X with Y = v; Z = w } } →  { r with X.Y = v; X.Z = w }
///
/// and recursively for deeper chains rooted at the same source.
///
/// Safety rules:
///   - the inner copy source must be exactly the outer source extended by
///     the field's own path (`r` → `r.X`), compared ident-by-ident — any
///     other source is a genuine cross-record copy and stays
///   - every flattened value is single-line (the values splice verbatim
///     into a `;`-joined field list)
///   - the path head must not collide with a TYPE name: in the flattened
///     syntax `{ r with Config.Value = v }`, a type named Config in scope
///     wins resolution over the field and the fix would not compile — the
///     `{ Config: Config }` field-named-after-its-type pattern is common,
///     so the field's own type name (typed check results) and every type
///     declared in the file are both checked
///   - the rule only reports when the project's language version allows
///     the syntax (gated at registration); no edit crosses a directive
module FSharp.Refactor.NestedRecordUpdate

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        /// The nested field being flattened (fix replaces this range).
        Range: range
        OriginalText: string
        ReplacementText: string
        /// e.g. "X.Y" for the message.
        Path: string
    }

/// The ident texts of a bare or dotted identifier expression.
let private identPath (e: SynExpr) =
    match e with
    | SynExpr.Ident id -> Some [ id.idText ]
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) -> Some(ids |> List.map (fun i -> i.idText))
    | _ -> None

/// Flatten record fields into (dotted-path, value-expr) leaves, walking
/// nested copy-and-updates whose source matches `basePath` + the field
/// path. Returns None when any part is not flattenable.
[<TailCall>]
let rec private flattenLoop
    (basePath: string list)
    (acc: (string * SynExpr) list)
    (pending: (string list * SynExprRecordField) list)
    : (string * SynExpr) list option =
    match pending with
    | [] -> Some(List.rev acc)
    | (prefix, field) :: rest ->
        match field with
        | SynExprRecordField(fieldName = (SynLongIdent(id = fieldIds), _); expr = Some value) ->
            let fieldPath = prefix @ (fieldIds |> List.map (fun i -> i.idText))

            match value with
            | SynExpr.Record(copyInfo = Some(innerBase, _); recordFields = innerFields) when
                identPath innerBase = Some(basePath @ fieldPath) && not innerFields.IsEmpty
                ->
                flattenLoop basePath acc ((innerFields |> List.map (fun f -> fieldPath, f)) @ rest)
            | _ when isSingleLine value.Range -> flattenLoop basePath ((String.concat "." fieldPath, value) :: acc) rest
            | _ -> None
        | _ -> None

let private flattenField (basePath: string list) (field: SynExprRecordField) = flattenLoop basePath [] [ [], field ]

/// The display name of the field's own type, resolved at the field ident.
let private fieldTypeName (check: FSharpCheckFileResults) (source: ISourceText) (fieldId: Ident) =
    let r = fieldId.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ fieldId.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpField as field ->
            try
                let t = field.FieldType

                if t.HasTypeDefinition then
                    Some t.TypeDefinition.DisplayName
                else
                    None
            with OptionModule.FcsSymbolFailure ->
                None
        | _ -> None
    | None -> None

/// Find flattenable nested copy-and-updates. Requires typed check results
/// for the type-name collision gate; the language version gate lives in
/// the registration.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    // type names declared in this file: a path head naming one of them
    // would resolve as the type, not the field
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

    [ for _, expr in index.Exprs do
          match expr with
          | SynExpr.Record(copyInfo = Some(outerBase, _); recordFields = fields) ->
              match identPath outerBase with
              | Some basePath ->
                  for field in fields do
                      match field with
                      // only fields whose value IS a nested copy-and-update
                      // are rewritten; sibling plain fields stay untouched
                      | SynExprRecordField(
                          fieldName = (SynLongIdent(id = fieldIds), _)
                          expr = Some(SynExpr.Record(copyInfo = Some _) as value)) when not fieldIds.IsEmpty ->
                          match flattenField basePath field with
                          // a leaf without a dot means the inner source did
                          // not match — a genuine cross-record copy, no gain
                          | Some leaves when
                              not leaves.IsEmpty
                              && leaves |> List.exists (fun (p, _) -> p.Contains '.')
                              // collision gate: the path head must not name
                              // a type
                              && (let head = (List.head fieldIds).idText

                                  not (fileTypeNames.Contains head)
                                  && fieldTypeName check source (List.head fieldIds) <> Some head)
                              ->
                              let fieldStart = (List.head fieldIds).idRange.Start

                              let editRange = Range.mkRange value.Range.FileName fieldStart value.Range.End

                              let replacement =
                                  leaves
                                  |> List.map (fun (path, v) -> $"{path} = {textOfRange source v.Range}")
                                  |> String.concat "; "

                              if not (spansDirective source editRange) then
                                  { Range = editRange
                                    OriginalText = textOfRange source editRange
                                    ReplacementText = replacement
                                    Path = leaves |> List.map fst |> String.concat ", " }
                          | _ -> ()
                      | _ -> ()
              | None -> ()
          | _ -> () ]
    // outermost wins: an inner copy-update also matches as its own outer,
    // but its rewrite is subsumed by the enclosing suggestion
    |> fun all ->
        all
        |> List.filter (fun s ->
            not (
                all
                |> List.exists (fun outer ->
                    not (obj.ReferenceEquals(outer, s))
                    && Range.rangeContainsRange outer.Range s.Range)
            ))
