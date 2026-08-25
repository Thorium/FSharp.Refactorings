/// Refactoring: remove `mutable` from a local binding that is never mutated.
///
///     let mutable x = 0        →     let x = 0
///     printfn "%d" x                 printfn "%d" x
///
/// Per the project policy, hints only point toward idiomatic F#, so only the
/// to-immutable direction is offered. Module-level and record-field
/// mutability are out of scope: fields can be assigned by serializers via
/// reflection and public module values from other files, which no single-file
/// analysis can prove absent.
///
/// Type-level `let mutable` fields ARE in scope (the ReSharper "field can be
/// readonly" rule): a class's let bindings are private to the type by
/// construction, so the whole mutation scope is the type definition itself.
///
/// Safety rules (checked over the binding's continuation, which is the whole
/// scope of a local):
///   - no assignment `x <-` (including `x.Field <-`) and no address-of `&x`,
///     checked conservatively on the source text so shadowing or string
///     contents can only suppress a hint, never produce a wrong one
///   - the binding's type must be a reference type, an enum, or a whitelisted
///     immutable value type: on other structs, removing `mutable` introduces
///     defensive copies that change the behavior of mutating members
module FSharp.Refactorings.MutableRemoval

open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

type Suggestion =
    {
        /// Range of the `mutable ` keyword (including trailing whitespace);
        /// the fix deletes it.
        Range: range
        OriginalText: string
        /// The bound variable's name, for the diagnostic message.
        Name: string
    }

/// Value types on which removing `mutable` cannot change behavior.
let private immutableValueTypes =
    set
        [ "System.Boolean"
          "System.Byte"
          "System.SByte"
          "System.Int16"
          "System.UInt16"
          "System.Int32"
          "System.UInt32"
          "System.Int64"
          "System.UInt64"
          "System.IntPtr"
          "System.UIntPtr"
          "System.Single"
          "System.Double"
          "System.Decimal"
          "System.Char"
          "System.DateTime"
          "System.DateTimeOffset"
          "System.TimeSpan"
          "System.Guid" ]

[<TailCall>]
let rec private stripAbbreviations (t: FSharpType) =
    if t.HasTypeDefinition && t.TypeDefinition.IsFSharpAbbreviation then
        stripAbbreviations t.TypeDefinition.AbbreviatedType
    else
        t

/// True when the local's type is safe to bind immutably: reference types,
/// enums, and whitelisted immutable structs.
let private typeAllowsRemoval (t: FSharpType) =
    let t = stripAbbreviations t

    if not t.HasTypeDefinition then
        // type parameters, tuples, functions: could be instantiated to a
        // mutable struct only via a generic local, which cannot be `mutable`
        // in a way that matters here — but stay conservative
        false
    else
        let td = t.TypeDefinition

        if td.IsEnum then
            true
        elif not td.IsValueType then
            true
        else
            td.TryFullName |> Option.exists immutableValueTypes.Contains

/// True when `body` may mutate `name`: an assignment (`name <-`,
/// `name.Field <-`, `name.[i] <-`) or an address-of (`&name`).
let private mayMutate (bodyText: string) (name: string) =
    let n = Regex.Escape name

    Regex.IsMatch(bodyText, @"\b" + n + @"(\.[^\n<]*|\[[^\n]*\]\s*)?\s*<-")
    || Regex.IsMatch(bodyText, @"&\s*" + n + @"\b")

/// The range of the `mutable` keyword plus its trailing whitespace, located
/// textually between the start of the let-binding and its head pattern.
let private mutableKeywordRange (source: ISourceText) (letStart: pos) (patStart: pos) (fileName: string) =
    if letStart.Line <> patStart.Line then
        None
    else
        let line = source.GetLineString(letStart.Line - 1)
        let segment = line.Substring(letStart.Column, patStart.Column - letStart.Column)
        let m = Regex.Match(segment, @"mutable\s+")

        if m.Success then
            let startCol = letStart.Column + m.Index

            Some(
                Range.mkRange
                    fileName
                    (Position.mkPos letStart.Line startCol)
                    (Position.mkPos letStart.Line (startCol + m.Length))
            )
        else
            None

/// Find local `let mutable` bindings that are never mutated. Requires typed
/// check results for the struct-safety gate.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()

    let resolvesToSafeType (ident: Ident) =
        let r = ident.idRange
        let lineText = source.GetLineString(r.EndLine - 1)

        match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
        | Some symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as value -> typeAllowsRemoval value.FullType
            | _ -> false
        | None -> false

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(_path, expr) =
                match expr with
                | SynExpr.LetOrUse lou when not lou.IsBang && not lou.IsUse ->
                    match lou.Bindings with
                    | [ SynBinding(isMutable = true; headPat = SynPat.Named(ident = SynIdent(ident = var)) as pat) ] when
                        not (mayMutate (textOfRange source lou.Body.Range) var.idText)
                        && resolvesToSafeType var
                        ->
                        match mutableKeywordRange source expr.Range.Start pat.Range.Start expr.Range.FileName with
                        | Some keywordRange ->
                            suggestions.Add
                                { Range = keywordRange
                                  OriginalText = textOfRange source keywordRange
                                  Name = var.idText }
                        | None -> ()
                    | _ -> ()
                | _ -> ()

            // type-level fields: a class's let bindings are private to the
            // type, so the mutation scope is the whole type definition
            override _.WalkSynModuleDecl(_path, decl) =
                match decl with
                | SynModuleDecl.Types(typeDefns = defns) ->
                    for SynTypeDefn(typeRepr = repr) as typeDefn in defns do
                        match repr with
                        | SynTypeDefnRepr.ObjectModel(members = members) ->
                            let typeText = lazy textOfRange source typeDefn.Range

                            for memberDefn in members do
                                match memberDefn with
                                | SynMemberDefn.LetBindings(
                                    bindings = [ SynBinding(
                                                     isMutable = true
                                                     headPat = SynPat.Named(ident = SynIdent(ident = var)) as pat) ]) when
                                    not (mayMutate typeText.Value var.idText) && resolvesToSafeType var
                                    ->
                                    match
                                        mutableKeywordRange
                                            source
                                            memberDefn.Range.Start
                                            pat.Range.Start
                                            memberDefn.Range.FileName
                                    with
                                    | Some keywordRange ->
                                        suggestions.Add
                                            { Range = keywordRange
                                              OriginalText = textOfRange source keywordRange
                                              Name = var.idText }
                                    | None -> ()
                                | _ -> ()
                        | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq suggestions
