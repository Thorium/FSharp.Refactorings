/// FR0119 (fix): a synchronous call inside `task { }` where the typed
/// tree proves an async twin exists — the preventive half of FR0049:
/// instead of flagging sync-over-async after the fact, take the
/// asynchronous road while the code is already in a task.
///
///     task {                                   task {
///         let line = reader.ReadLine()   →         let! line = reader.ReadLineAsync()
///         writer.Flush()                           do! writer.FlushAsync()
///
/// Typed gates:
///   - the resolved method's declaring entity offers `<Name>Async` with
///     the SAME parameter-type prefix (an extra trailing OPTIONAL
///     CancellationToken is fine — FR0118 hands it the token on the next
///     pass) and a Task-shaped return: `T` → `Task<T>`/`ValueTask<T>`,
///     `unit` → `Task`/`ValueTask`
///   - `let x = ...` bindings become `let!` (simple named pattern, no
///     annotation); statement position becomes `do!` only when the twin
///     returns the NON-generic Task
///   - inside `async { }` the same rewrite bridges with
///     `|> Async.AwaitTask` — real Task twins only there, AwaitTask has
///     no ValueTask overload
///   - never inside a lambda, a nested CE, a finally block or an
///     exception handler
module FSharp.Refactor.AwaitableOverload

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        Range: range
        /// (range, original, replacement) edits: the binding keyword or
        /// statement prefix, plus the method name gaining its suffix.
        Fixes: (range * string * string) list
        MethodName: string
    }

let private ceBuilders = set [ "task"; "backgroundTask"; "async" ]

[<return: Struct>]
let private (|CallIdent|_|) (e: SynExpr) =
    match e with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> ValueSome(List.last ids)
    | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> ValueSome(List.last ids)
    | _ -> ValueNone

let private parameterShapes (displayContext: FSharpDisplayContext) (mfv: FSharpMemberOrFunctionOrValue) =
    try
        match mfv.CurriedParameterGroups |> List.ofSeq with
        | [ group ] -> Some [ for p in group -> p.Type.Format displayContext ]
        | _ -> None
    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
        None

let private isOptionalCancellationToken (m: FSharpMemberOrFunctionOrValue) =
    try
        let last = m.CurriedParameterGroups |> Seq.collect id |> Seq.last

        last.IsOptionalArg
        && (match last.Type.StripAbbreviations().TypeDefinition.TryFullName with
            | Some full -> full = "System.Threading.CancellationToken"
            | None -> false)
    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
        false

let private returnFullName (m: FSharpMemberOrFunctionOrValue) =
    try
        match m.ReturnParameter.Type.StripAbbreviations().TypeDefinition.TryFullName with
        | Some full -> full
        | None -> ""
    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
        ""

/// The async twin's return must wrap the original's: `T` → `Task<T>` /
/// `ValueTask<T>`, `unit` → `Task`/`ValueTask`. Compared structurally —
/// formatted names depend on what happens to be open at the call site.
let private returnsWrapped
    (displayContext: FSharpDisplayContext)
    (orig: FSharpMemberOrFunctionOrValue)
    (twin: FSharpMemberOrFunctionOrValue)
    =
    try
        let origFormat = orig.ReturnParameter.Type.Format displayContext

        match returnFullName twin with
        | "System.Threading.Tasks.Task"
        | "System.Threading.Tasks.ValueTask" -> origFormat = "unit"
        | n when
            n.StartsWith "System.Threading.Tasks.Task`"
            || n.StartsWith "System.Threading.Tasks.ValueTask`"
            ->
            twin.ReturnParameter.Type.StripAbbreviations().GenericArguments
            |> Seq.tryHead
            |> Option.map (fun inner -> inner.Format displayContext = origFormat)
            |> Option.defaultValue false
        | _ -> false
    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
        false

/// Does the twin return the NON-generic Task — the only shape `do!` binds.
let private returnsPlainTask (twin: FSharpMemberOrFunctionOrValue) =
    match returnFullName twin with
    | "System.Threading.Tasks.Task"
    | "System.Threading.Tasks.ValueTask" -> true
    | _ -> false

/// Async.AwaitTask exists for real Tasks only.
let private returnsRealTask (twin: FSharpMemberOrFunctionOrValue) =
    let n = returnFullName twin
    n = "System.Threading.Tasks.Task" || n.StartsWith "System.Threading.Tasks.Task`"

let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        // task/async CE bodies with their builder, for scoping and for
        // picking the bind bridge
        let ces =
            index.Exprs
            |> Array.choose (fun (_, e) ->
                match e with
                | SynExpr.App(
                    isInfix = false; funcExpr = IdentName builder; argExpr = SynExpr.ComputationExpr(expr = body)) when
                    ceBuilders.Contains builder
                    ->
                    Some(builder, body.Range)
                | _ -> None)

        let lambdaRanges =
            index.Exprs
            |> Array.collect (fun (_, e) ->
                match e with
                | SynExpr.Lambda _
                | SynExpr.MatchLambda _ -> [| e.Range |]
                // a LOCAL FUNCTION's body is a closure too, but its AST is
                // a binding with argument patterns, not a Lambda node — a
                // do!/let! injected there would land in a plain function
                | LetOrUseE lou when not lou.IsBang ->
                    lou.Bindings
                    |> List.choose (fun b ->
                        match b with
                        | SynBinding(headPat = SynPat.LongIdent(argPats = SynArgPats.Pats(_ :: _))) ->
                            Some b.RangeOfBindingWithRhs
                        | _ -> None)
                    |> Array.ofList
                | _ -> [||])

        let otherCeRanges =
            index.Exprs
            |> Array.choose (fun (_, e) ->
                match e with
                | SynExpr.ComputationExpr(expr = body) -> Some body.Range
                | SynExpr.ArrayOrListComputed(expr = body) -> Some body.Range
                | _ -> None)

        let noBindRanges =
            index.Exprs
            |> Array.collect (fun (_, e) ->
                match e with
                | SynExpr.TryFinally(finallyExpr = f) -> [| f.Range |]
                | SynExpr.TryWith(withCases = cases) ->
                    cases
                    |> List.map (fun (SynMatchClause(resultExpr = result)) -> result.Range)
                    |> Array.ofList
                | _ -> [||])

        // the INNERMOST enclosing task/async body a site sits directly in:
        // its builder decides the bridge. Nothing inside a lambda, nested
        // CE, finally, or handler qualifies
        let directBuilder (r: range) =
            if noBindRanges |> Array.exists (fun z -> Range.rangeContainsRange z r) then
                None
            else
                ces
                |> Array.filter (fun (_, ceRange) ->
                    Range.rangeContainsRange ceRange r
                    && not (
                        lambdaRanges
                        |> Array.exists (fun l -> Range.rangeContainsRange ceRange l && Range.rangeContainsRange l r)
                    )
                    && not (
                        otherCeRanges
                        |> Array.exists (fun other ->
                            Range.rangeContainsRange ceRange other
                            && not (Range.equals other ceRange)
                            && Range.rangeContainsRange other r)
                    ))
                |> Array.sortBy (fun (_, ceRange) -> ceRange.EndLine - ceRange.StartLine, ceRange.EndColumn)
                |> Array.tryHead
                |> Option.map fst

        // the plain `let` whose entire RHS is `target` — the let! shape
        let bindingKeywordFor (target: range) =
            index.Exprs
            |> Array.tryPick (fun (_, e) ->
                match e with
                | LetOrUseE lou when not (lou.IsBang || lou.IsUse || lou.IsRecursive) ->
                    match lou.Bindings with
                    | [ SynBinding(
                            isMutable = false
                            returnInfo = None
                            headPat = SynPat.Named _
                            expr = rhs
                            trivia = btrivia) ] when Range.equals rhs.Range target -> Some btrivia.LeadingKeyword.Range
                    | _ -> None
                | _ -> None)

        // statement position: a direct sequential element of the CE chain
        let isStatement (target: range) =
            index.Exprs
            |> Array.exists (fun (_, e) ->
                match e with
                | SynExpr.Sequential(expr1 = a) -> Range.equals a.Range target
                | _ -> false)

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.App(isInfix = false; funcExpr = CallIdent methodId; argExpr = args) when
                  not (methodId.idText.EndsWith "Async")
                  ->
                  let tupled =
                      match args with
                      | SynExpr.Const(SynConst.Unit, _) -> Some 0
                      | SynExpr.Paren(expr = SynExpr.Tuple(exprs = es)) -> Some es.Length
                      | SynExpr.Paren _ -> Some 1
                      // juxtaposed atomic argument — `writer.Write s` — is
                      // the common F# spelling; the rewrite only touches the
                      // keyword and the name, so the arg shape can stay
                      | SynExpr.Const _
                      | SynExpr.Ident _
                      | SynExpr.LongIdent _ -> Some 1
                      | _ -> None

                  match tupled, directBuilder expr.Range with
                  | Some arity, Some builder ->
                      let lineText = source.GetLineString(methodId.idRange.EndLine - 1)

                      let resolved =
                          check.GetSymbolUseAtLocation(
                              methodId.idRange.EndLine,
                              methodId.idRange.EndColumn,
                              lineText,
                              [ methodId.idText ]
                          )

                      match resolved with
                      | Some symbolUse ->
                          match symbolUse.Symbol with
                          | :? FSharpMemberOrFunctionOrValue as mfv when mfv.IsMember && not mfv.IsProperty ->
                              let ctx = symbolUse.DisplayContext

                              let twin =
                                  match parameterShapes ctx mfv with
                                  | Some ps when ps.Length = arity ->
                                      (try
                                          match mfv.DeclaringEntity with
                                          | Some entity ->
                                              entity.MembersFunctionsAndValues
                                              |> Seq.tryFind (fun m ->
                                                  m.DisplayName = mfv.DisplayName + "Async"
                                                  && returnsWrapped ctx mfv m
                                                  && (match parameterShapes ctx m with
                                                      | Some mps when mps.Length = arity -> mps = ps
                                                      | Some mps when mps.Length = arity + 1 ->
                                                          List.truncate arity mps = ps && isOptionalCancellationToken m
                                                      | _ -> false))
                                          | None -> None
                                       with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                                           None)
                                  | _ -> None

                              // async { } bridges via Async.AwaitTask, which
                              // has no ValueTask overload — real Task only
                              let twin =
                                  match builder, twin with
                                  | "async", Some m when not (returnsRealTask m) -> None
                                  | _ -> twin

                              match twin with
                              | Some twinM ->
                                  let renameFix = methodId.idRange, methodId.idText, methodId.idText + "Async"

                                  let bridgeFixes =
                                      if builder = "async" then
                                          let atEnd = Range.mkRange expr.Range.FileName expr.Range.End expr.Range.End

                                          [ atEnd, "", " |> Async.AwaitTask" ]
                                      else
                                          []

                                  match bindingKeywordFor expr.Range with
                                  | Some kw when textOfRange source kw = "let" ->
                                      { Range = expr.Range
                                        Fixes = [ kw, "let", "let!"; renameFix ] @ bridgeFixes
                                        MethodName = methodId.idText }
                                  | Some _ -> ()
                                  | None ->
                                      // statement position takes do! — but
                                      // only a NON-generic Task binds there
                                      if isStatement expr.Range && returnsPlainTask twinM then
                                          let at = Range.mkRange expr.Range.FileName expr.Range.Start expr.Range.Start

                                          { Range = expr.Range
                                            Fixes = [ at, "", "do! "; renameFix ] @ bridgeFixes
                                            MethodName = methodId.idText }
                              | None -> ()
                          | _ -> ()
                      | None -> ()
                  | _ -> ()
              | _ -> () ]
