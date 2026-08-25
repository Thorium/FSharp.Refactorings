/// Refactoring notes for runtime type comparisons (advice only):
///
///     x.GetType().Name = "Customer"        // fragile string comparison:
///                                          // renames, namespaces, generics
///                                          // all break it silently
///     x.GetType() = typeof<Customer>       // exact-type equality; often a
///                                          // type test `x :? Customer` was
///                                          // meant (which matches subtypes)
///
/// The first shape gets a "compare types, not names" note. The second gets
/// a note offering `:?` with the exact-vs-subtype caveat spelled out — the
/// two are NOT equivalent, so there is no automatic fix.
module FSharp.Refactorings.TypeChecks

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

[<RequireQualifiedAccess>]
type TypeCheckKind =
    /// `x.GetType().Name = "..."` / `.FullName = "..."`.
    | NameComparison of property: string
    /// `x.GetType() = typeof<T>`.
    | TypeofEquality of receiverText: string * typeText: string

type Suggestion = { Range: range; Kind: TypeCheckKind }

/// `<receiver>.GetType()` — the receiver expression.
[<return: Struct>]
let private (|GetTypeCall|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = UnitConst) when
        ids.Length >= 2 && (List.last ids).idText = "GetType"
        ->
        ValueSome()
    | SynExpr.App(
        isInfix = false; funcExpr = SynExpr.DotGet(longDotId = SynLongIdent(id = [ getTypeId ])); argExpr = UnitConst) when
        getTypeId.idText = "GetType"
        ->
        ValueSome()
    | _ -> ValueNone

/// `<receiver>.GetType().Name` / `.FullName` — the property name.
[<return: Struct>]
let private (|TypeNameAccess|_|) (e: SynExpr) =
    match e with
    | SynExpr.DotGet(expr = GetTypeCall; longDotId = SynLongIdent(id = [ propId ])) when
        propId.idText = "Name" || propId.idText = "FullName"
        ->
        ValueSome propId.idText
    | _ -> ValueNone

[<return: Struct>]
let private (|StringLiteral|_|) (e: SynExpr) =
    match e with
    | SynExpr.Const(SynConst.String _, _) -> ValueSome()
    | _ -> ValueNone

/// `typeof<T>` — the type argument's source text.
[<return: Struct>]
let private (|TypeofExpr|_|) (e: SynExpr) =
    match e with
    | SynExpr.TypeApp(expr = IdentName "typeof"; typeArgs = [ t ]) -> ValueSome t.Range
    | _ -> ValueNone

/// Find fragile runtime type comparisons.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    [ for _, expr in index.Exprs do
          match expr with
          | SynExpr.App(funcExpr = SynExpr.App(funcExpr = IdentName "op_Equality"; argExpr = lhs); argExpr = rhs) ->
              match lhs, rhs with
              | TypeNameAccess prop, StringLiteral
              | StringLiteral, TypeNameAccess prop ->
                  { Range = expr.Range
                    Kind = TypeCheckKind.NameComparison prop }
              | (GetTypeCall as getTypeSide), TypeofExpr typeRange
              | TypeofExpr typeRange, (GetTypeCall as getTypeSide) ->
                  let receiverText =
                      // strip the trailing `.GetType()` for the message
                      let text = textOfRange source getTypeSide.Range
                      let cut = text.LastIndexOf ".GetType"
                      if cut > 0 then text.Substring(0, cut) else text

                  { Range = expr.Range
                    Kind = TypeCheckKind.TypeofEquality(receiverText, textOfRange source typeRange) }
              | _ -> ()
          | _ -> () ]
