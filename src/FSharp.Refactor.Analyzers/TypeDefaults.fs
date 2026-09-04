/// The value a type makes obvious, and the zero it has otherwise — shared
/// by the rules that fill a gap the compiler reports: a record's missing
/// fields (FR0145), an object expression's missing members (FR0077).
///
/// "Obvious" is the empty value a field or member would have had before
/// it existed: `None`, `[]`, `Map.empty`, `()`. A sweep may write those.
/// "Zero" is the value a type has when nothing is chosen — `false`, `0`,
/// `""`, `Guid.Empty`, `Unchecked.defaultof<_>` — and is an editor's
/// offer, never a sweep's: a null or a zero is exactly the silent default
/// an automatic rewrite must not pick.
module FSharp.Refactor.TypeDefaults

open FSharp.Compiler.Symbols

let private fullName (t: FSharpType) =
    try
        let t = if t.IsAbbreviation then t.AbbreviatedType else t

        if t.HasTypeDefinition then
            t.TypeDefinition.TryFullName
        else
            None
    with _ -> // fsharpanalyzer: ignore-line FR0055
        None

/// The empty value a type makes obvious, if any.
let obviousDefault (t: FSharpType) : string option =
    try
        let t = if t.IsAbbreviation then t.AbbreviatedType else t

        if t.IsFunctionType || t.IsTupleType then
            None
        else
            match fullName t with
            | Some "Microsoft.FSharp.Core.FSharpOption`1" -> Some "None"
            | Some "Microsoft.FSharp.Core.FSharpValueOption`1" -> Some "ValueNone"
            | Some "Microsoft.FSharp.Collections.FSharpList`1" -> Some "[]"
            | Some "Microsoft.FSharp.Collections.FSharpMap`2" -> Some "Map.empty"
            | Some "Microsoft.FSharp.Collections.FSharpSet`1" -> Some "Set.empty"
            | Some "System.Collections.Generic.IEnumerable`1" -> Some "Seq.empty"
            | Some("Microsoft.FSharp.Core.Unit" | "Microsoft.FSharp.Core.unit") -> Some "()"
            | Some "System.Array" -> Some "[||]"
            | _ when t.HasTypeDefinition && t.TypeDefinition.IsArrayType -> Some "[||]"
            | _ -> None
    with _ -> // fsharpanalyzer: ignore-line FR0055
        None

/// The zero of a primitive type as F# spells it, the well-known empty of a
/// few structs, `Unchecked.defaultof<_>` for everything else.
let zeroDefault (t: FSharpType) : string =
    match fullName t with
    | Some "System.Boolean" -> "false"
    | Some("System.Int32" | "System.Int64" | "System.Int16" | "System.Byte" | "System.SByte" | "System.UInt32" | "System.UInt64" | "System.UInt16") ->
        "0"
    | Some("System.Double" | "System.Single") -> "0.0"
    | Some "System.Decimal" -> "0m"
    | Some "System.String" -> "\"\""
    | Some "System.Char" -> "' '"
    | Some "System.Guid" -> "System.Guid.Empty"
    | Some "System.DateTime" -> "System.DateTime.MinValue"
    | Some "System.TimeSpan" -> "System.TimeSpan.Zero"
    | _ -> "Unchecked.defaultof<_>"

/// The obvious empty value, or the zero when there is none.
let emptyValue (t: FSharpType) : string =
    match obviousDefault t with
    | Some v -> v
    | None -> zeroDefault t
