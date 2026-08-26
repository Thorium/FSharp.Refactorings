/// Three allocation hints for CONTAINED types, in the spirit of
/// https://www.bartoszsypytkowski.com/writing-high-performance-f-code/ :
///
/// 1. voption fields (FR0069): a private/internal record field typed
///    `int option` / `DateTime option` / `Guid option` heap-allocates a
///    Some box around a struct payload; `voption` (ValueOption) keeps the
///    value flat in the record.
///
/// 2. Small struct types (FR0070): a private/internal record whose fields
///    are all small structs (four fields at most) can carry [<Struct>],
///    removing one heap allocation per instance.
///
/// 3. Struct tuple fields (FR0093): a field typed `int * int` is a
///    System.Tuple — a heap object per value — while `struct (int * int)`
///    is a ValueTuple stored inline, in the same spirit as 1.
///
/// Both are deliberately gated to private/internal types — declared so, or
/// living inside a private/internal module. For a PUBLIC type the
/// migration is much bigger than the type itself: serialized shapes may
/// change, and the signature ripples into an unbounded amount of call-site
/// refactoring. Contained visibility keeps the blast radius in one file or
/// one assembly. See Visibility.isInScope: `--api-changes` opts into the
/// public case deliberately.
module FSharp.Refactorings.StructHints

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

type VOptionFieldSuggestion =
    {
        Range: range
        FieldName: string
        /// The struct payload's type text, e.g. "int".
        ElementText: string
        TypeName: string
    }

type StructTypeSuggestion =
    { Range: range
      TypeName: string
      FieldCount: int }

type StructTupleFieldSuggestion =
    {
        Range: range
        FieldName: string
        /// The tuple's own source text, e.g. "int * int".
        TupleText: string
        TypeName: string
    }

/// Well-known struct types by (last) name — the primitives plus the
/// common BCL value types the article calls out.
let private structNames =
    set
        [ "int"
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
          "nativeint"
          "unativeint"
          "DateTime"
          "DateTimeOffset"
          "TimeSpan"
          "Guid" ]

let private lastIdentText (t: SynType) =
    match t with
    | SynType.LongIdent(SynLongIdent(id = ids)) when not ids.IsEmpty -> Some (List.last ids).idText
    | _ -> None

/// A type whose name is a known struct.
let private isKnownStruct (t: SynType) =
    lastIdentText t |> Option.exists structNames.Contains

/// `<struct> option`, in postfix or prefix form.
[<return: Struct>]
let private (|StructOption|_|) (t: SynType) =
    match t with
    | SynType.App(typeName = name; typeArgs = [ elem ]) when
        (lastIdentText name = Some "option" || lastIdentText name = Some "Option")
        && isKnownStruct elem
        ->
        ValueSome elem
    | _ -> ValueNone

/// A reference tuple of a handful of known structs: `int * int`.
///
/// Capped at four elements: a struct tuple is copied by value, so past a
/// few small fields the copying costs more than the allocation it saves.
/// Already-struct tuples are left alone, as are tuples carrying a `/`
/// segment (units of measure), where the segments are not plain elements.
[<return: Struct>]
let private (|SmallStructTuple|_|) (t: SynType) =
    match t with
    | SynType.Tuple(isStruct = false; path = segments) when
        segments
        |> List.forall (function
            | SynTupleTypeSegment.Slash _ -> false
            | _ -> true)
        ->
        let elements =
            segments
            |> List.choose (function
                | SynTupleTypeSegment.Type element -> Some element
                | _ -> None)

        if
            elements.Length >= 2
            && elements.Length <= 4
            && elements |> List.forall isKnownStruct
        then
            ValueSome elements
        else
            ValueNone
    | _ -> ValueNone

/// A field type that already lives flat: a known struct, or a voption of one.
let private isStructField (t: SynType) =
    isKnownStruct t
    || (match t with
        | SynType.App(typeName = name; typeArgs = [ elem ]) when
            (lastIdentText name = Some "voption" || lastIdentText name = Some "ValueOption")
            && isKnownStruct elem
            ->
            true
        | _ -> false)

/// Find all three hint kinds. Parse-only.
let find
    (allowApiChanges: bool)
    (parseTree: ParsedInput)
    (source: ISourceText)
    : VOptionFieldSuggestion list * StructTypeSuggestion list * StructTupleFieldSuggestion list =
    let index = AstIndex.ofTree parseTree
    let voptions = ResizeArray<VOptionFieldSuggestion>()
    let structs = ResizeArray<StructTypeSuggestion>()
    let structTuples = ResizeArray<StructTupleFieldSuggestion>()

    for path, decl in index.Decls do
        match decl with
        | SynModuleDecl.Types(typeDefns = defns) ->
            for SynTypeDefn(typeInfo = info; typeRepr = repr) in defns do
                let (SynComponentInfo(attributes = attrs; longId = typeIds; accessibility = access)) =
                    info

                let typeName = typeIds |> List.map (fun i -> i.idText) |> String.concat "."

                match repr with
                | SynTypeDefnRepr.Simple(simpleRepr = SynTypeDefnSimpleRepr.Record(recordFields = fields)) when
                    Visibility.isInScope allowApiChanges path [ access ]
                    ->
                    // FR0069: struct payloads boxed in option fields
                    // FR0093: struct payloads boxed in a reference tuple
                    for SynField(idOpt = idOpt; fieldType = fieldType) in fields do
                        match fieldType, idOpt with
                        | StructOption elem, Some fieldId ->
                            voptions.Add
                                { Range = fieldType.Range
                                  FieldName = fieldId.idText
                                  ElementText = textOfRange source elem.Range
                                  TypeName = typeName }
                        | SmallStructTuple _, Some fieldId ->
                            structTuples.Add
                                { Range = fieldType.Range
                                  FieldName = fieldId.idText
                                  TupleText = textOfRange source fieldType.Range
                                  TypeName = typeName }
                        | _ -> ()

                    // FR0070: a small all-struct record can be a struct itself
                    let alreadyStruct = hasAttributeNamed "Struct" attrs

                    let allStructFields =
                        fields
                        |> List.forall (fun (SynField(fieldType = t; isMutable = m)) -> not m && isStructField t)

                    if
                        not alreadyStruct
                        && not fields.IsEmpty
                        && fields.Length <= 4
                        && allStructFields
                        && not typeIds.IsEmpty
                    then
                        structs.Add
                            { Range = (List.last typeIds).idRange
                              TypeName = typeName
                              FieldCount = fields.Length }
                | _ -> ()
        | _ -> ()

    List.ofSeq voptions, List.ofSeq structs, List.ofSeq structTuples
