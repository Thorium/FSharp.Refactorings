/// Refactoring note (correctness, CA2002): locking on an object with weak
/// identity synchronizes with strangers.
///
///     lock "cache" (fun () -> ...)      // interned strings are shared
///     lock typeof<T> (fun () -> ...)    // runtime Type objects are shared
///     lock (x.GetType()) (fun () -> ...)
///
/// Interned strings and runtime Type objects are process-wide singletons:
/// any other code locking the same string content or the same type takes
/// the same monitor, inviting contention and deadlocks that no local
/// reasoning can rule out. The remedy is a dedicated private lock object
/// (`let lockObj = obj ()`), which is the author's structural decision —
/// so this is advice without a fix.
///
/// The `lock` function is typed-gated to FSharp.Core; string-typed
/// identifiers resolve via the check results.
module FSharp.Refactorings.WeakLock

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

[<RequireQualifiedAccess>]
type WeakKind =
    | StringValue
    | TypeObject

type Suggestion =
    {
        Range: range
        Kind: WeakKind
        /// The locked expression's text, for the message.
        TargetText: string
    }

let private resolvesToString (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value ->
            try
                let t = OptionModule.stripAbbreviations value.FullType
                t.HasTypeDefinition && t.TypeDefinition.TryFullName = Some "System.String"
            with _ ->
                false
        | _ -> false
    | None -> false

/// `x.GetType()` in either parse shape.
[<return: Struct>]
let private (|GetTypeCall|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = UnitConst) when
        ids.Length >= 2 && (List.last ids).idText = "GetType"
        ->
        ValueSome()
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.DotGet(longDotId = SynLongIdent(id = [ id ])); argExpr = UnitConst) when
        id.idText = "GetType"
        ->
        ValueSome()
    | _ -> ValueNone

/// Find locks on weak-identity objects. Requires typed check results.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.App(
                  isInfix = false
                  funcExpr = SynExpr.App(isInfix = false; funcExpr = SingleIdent lockId; argExpr = lockObj)
                  argExpr = _) when lockId.idText = "lock" ->
                  let weak =
                      match stripParens lockObj with
                      | SynExpr.Const(SynConst.String _, _) -> Some WeakKind.StringValue
                      | SynExpr.TypeApp(expr = IdentName "typeof") -> Some WeakKind.TypeObject
                      | GetTypeCall -> Some WeakKind.TypeObject
                      | SynExpr.Ident id when resolvesToString check source id -> Some WeakKind.StringValue
                      | _ -> None

                  match weak with
                  | Some kind when OptionModule.resolvesToCoreOperator check source lockId ->
                      { Range = expr.Range
                        Kind = kind
                        TargetText = textOfRange source (stripParens lockObj).Range }
                  | _ -> ()
              | _ -> () ]
