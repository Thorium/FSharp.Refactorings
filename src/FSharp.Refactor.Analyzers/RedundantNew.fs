/// Refactoring (FR0085, the ReSharper redundant-`new`): F# convention
/// reserves the `new` keyword for constructions the code must dispose —
/// the compiler itself warns the other way around (FS0760) when an
/// IDisposable is constructed WITHOUT `new`.
///
///     let sb = new StringBuilder()     →  let sb = StringBuilder()
///     use fs = new FileStream(...)     // stays: disposable, new belongs
///
/// Typed-gated: the constructed type must resolve and NOT implement
/// IDisposable; unresolved types stay untouched.
module FSharp.Refactor.RedundantNew

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    { Range: range
      OriginalText: string
      TypeName: string }

let private disposableNames =
    set [ "System.IDisposable"; "System.IAsyncDisposable" ]

let private isDisposableEntity (entity: FSharpEntity) =
    try
        (entity.TryFullName |> Option.exists disposableNames.Contains)
        || entity.AllInterfaces
           |> Seq.exists (fun i ->
               i.HasTypeDefinition
               && (i.TypeDefinition.TryFullName |> Option.exists disposableNames.Contains))
    with OptionModule.FcsSymbolFailure ->
        true // unknown: assume disposable, keep the `new`

let private resolvesToNonDisposable (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpEntity as entity -> not (isDisposableEntity entity)
        | :? FSharpMemberOrFunctionOrValue as value when value.IsConstructor ->
            try
                value.ApparentEnclosingEntity |> Option.exists (isDisposableEntity >> not)
            with OptionModule.FcsSymbolFailure ->
                false
        | _ -> false
    | None -> false

/// Find `new` on non-disposable constructions. Requires typed check
/// results.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.New(targetType = targetType) ->
                  let typeIdent =
                      match targetType with
                      | SynType.LongIdent(SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
                      | SynType.App(typeName = SynType.LongIdent(SynLongIdent(id = ids))) when not ids.IsEmpty ->
                          Some(List.last ids)
                      | _ -> None

                  match typeIdent with
                  | Some typeIdent when resolvesToNonDisposable check source typeIdent ->
                      // the `new ` keyword: from the expression start to the
                      // type's start
                      let newRange =
                          Range.mkRange expr.Range.FileName expr.Range.Start targetType.Range.Start

                      let newText = textOfRange source newRange

                      if newText.TrimEnd() = "new" then
                          { Range = newRange
                            OriginalText = newText
                            TypeName = typeIdent.idText }
                  | _ -> ()
              | _ -> () ]
