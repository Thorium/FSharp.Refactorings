/// Refactoring (correctness/perf, CA1849 + the VSTHRD family): blocking
/// waits inside an async or task computation expression.
///
///     task {
///         let x = t.Result                      // blocks the thread
///         other.Wait()                          // blocks
///         let y = (f ()).GetAwaiter().GetResult()   // blocks
///         let z = comp |> Async.RunSynchronously    // blocks
///         Thread.Sleep 100                      // blocks
///         ...
///     }
///
/// Sync-over-async inside a CE holds a thread-pool thread while awaiting
/// work that wants those same threads — the classic starvation/deadlock
/// recipe. The bind forms (`let!`, `do!`, `Async.AwaitTask`) release the
/// thread instead.
///
/// `Thread.Sleep n` in statement position gets a fix (`do! Async.Sleep n`
/// in async, `do! Task.Delay n` in task); the other shapes are advice —
/// rewriting them to binds restructures the surrounding code.
///
/// All receivers/methods are typed-gated (Task.Result, Task.Wait, an
/// awaiter's GetResult, FSharp.Core's RunSynchronously, Thread.Sleep), so
/// a user type with a `Result` property never matches. Only the innermost
/// enclosing CE reports a site, and no fix is offered inside a lambda
/// (where `do!` would not compile).
module FSharp.Refactor.SyncOverAsync

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

[<RequireQualifiedAccess>]
type BlockKind =
    | TaskResult
    | TaskWait
    | AwaiterGetResult
    | RunSynchronously
    | ThreadSleep

type Suggestion =
    {
        Range: range
        Kind: BlockKind
        /// The enclosing builder ("async"/"task"/"backgroundTask"), or None
        /// when the site is outside any CE (GetResult only — it is an
        /// antipattern everywhere).
        Builder: string option
        /// Present for Thread.Sleep in statement position.
        Fix: (range * string * string) option
    }

let private ceBuilders = set [ "async"; "task"; "backgroundTask" ]

/// Does the identifier's enclosing entity satisfy the predicate?
/// Task and ValueTask both block on .Result/.Wait — the BCL's async I/O
/// returns ValueTask everywhere post-core, so a Task-only prefix test
/// missed most modern blocking sites.
let private taskFamily (entity: string) =
    entity.StartsWith "System.Threading.Tasks.Task"
    || entity.StartsWith "System.Threading.Tasks.ValueTask"

let private enclosingEntityOf (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value -> OptionModule.enclosingFullName value
        | _ -> ""
    | None -> ""

let private fullNameOf (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as value -> OptionModule.fullNameOf value
        | _ -> ""
    | None -> ""

/// The last identifier of a member-call function expression.
[<return: Struct>]
let private (|CallIdent|_|) (e: SynExpr) =
    match e with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when ids.Length >= 2 -> ValueSome(List.last ids)
    | SynExpr.DotGet(longDotId = SynLongIdent(id = [ id ])) -> ValueSome id
    | _ -> ValueNone

/// Find blocking calls inside async/task CEs. Requires typed check results.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        // every async/task CE body, for innermost-attribution
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

        let innermostCe (r: range) =
            ces
            |> Array.filter (fun (_, ceRange) -> Range.rangeContainsRange ceRange r)
            |> Array.sortBy (fun (_, ceRange) -> ceRange.EndLine - ceRange.StartLine, ceRange.EndColumn)
            |> Array.tryHead

        let lambdaRanges =
            index.Exprs
            |> Array.choose (fun (_, e) ->
                match e with
                | SynExpr.Lambda _
                | SynExpr.MatchLambda _ -> Some e.Range
                | _ -> None)

        let insideLambdaWithin (ceRange: range) (r: range) =
            lambdaRanges
            |> Array.exists (fun l -> Range.rangeContainsRange ceRange l && Range.rangeContainsRange l r)

        // every CE body of ANY builder (seq { }, query { }, custom ones) and
        // every comprehension. A `do!` fix landing in statement position of
        // one of those nested inside the async/task would call a Bind the
        // builder does not have — a compile error, not a fix.
        let otherCeRanges =
            index.Exprs
            |> Array.choose (fun (_, e) ->
                match e with
                | SynExpr.ComputationExpr(expr = body) -> Some body.Range
                | SynExpr.ArrayOrListComputed(expr = body) -> Some body.Range
                | _ -> None)

        let insideOtherCeWithin (ceRange: range) (r: range) =
            otherCeRanges
            |> Array.exists (fun other ->
                Range.rangeContainsRange ceRange other
                && not (Range.equals other ceRange)
                && Range.rangeContainsRange other r)

        [ for path, expr in index.Exprs do
              let blocking =
                  match expr with
                  // t.Result — a bare property path or a dot-get
                  | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
                      ids.Length >= 2 && (List.last ids).idText = "Result"
                      ->
                      let id = List.last ids

                      if (enclosingEntityOf check source id) |> taskFamily then
                          Some(BlockKind.TaskResult, None)
                      else
                          None
                  | SynExpr.DotGet(longDotId = SynLongIdent(id = [ id ])) when
                      id.idText = "Result" && (enclosingEntityOf check source id) |> taskFamily
                      ->
                      Some(BlockKind.TaskResult, None)
                  | SynExpr.App(isInfix = false; funcExpr = CallIdent id) when
                      (id.idText = "Wait" || id.idText = "WaitAll" || id.idText = "WaitAny")
                      && (enclosingEntityOf check source id) |> taskFamily
                      ->
                      Some(BlockKind.TaskWait, None)
                  | SynExpr.App(isInfix = false; funcExpr = CallIdent id; argExpr = UnitConst) when
                      id.idText = "GetResult"
                      && (enclosingEntityOf check source id).Contains "Awaiter"
                      ->
                      Some(BlockKind.AwaiterGetResult, None)
                  | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
                      pathEndsWith "Async" "RunSynchronously" ids
                      && (fullNameOf check source (List.last ids)).StartsWith "Microsoft.FSharp.Control"
                      ->
                      Some(BlockKind.RunSynchronously, None)
                  | SynExpr.App(
                      isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = arg) when
                      pathEndsWith "Thread" "Sleep" ids
                      && (enclosingEntityOf check source (List.last ids)) = "System.Threading.Thread"
                      ->
                      Some(BlockKind.ThreadSleep, Some arg)
                  | _ -> None

              match blocking with
              | Some(kind, sleepArg) ->
                  match innermostCe expr.Range with
                  | None when kind <> BlockKind.ThreadSleep && kind <> BlockKind.RunSynchronously ->
                      // sync-over-async at a boundary: an antipattern even
                      // outside CEs — either the caller becomes async (wrap
                      // in task { } and bind) or the synchronous API should
                      // be used. Thread.Sleep in sync code is legitimate,
                      // and Async.RunSynchronously outside a CE IS F#'s
                      // intended sync-boundary runner.
                      { Range = expr.Range
                        Kind = kind
                        Builder = None
                        Fix = None }
                  | Some(builder, ceRange) ->
                      let fix =
                          match kind, sleepArg with
                          | BlockKind.ThreadSleep, Some arg when
                              not (insideLambdaWithin ceRange expr.Range)
                              && not (insideOtherCeWithin ceRange expr.Range)
                              ->
                              // statement position: a sequential element, the
                              // CE body itself, a let-continuation, or `do ...`
                              let target =
                                  match path with
                                  | SyntaxNode.SynExpr(SynExpr.Do _ as doExpr) :: _ -> Some doExpr.Range
                                  | SyntaxNode.SynExpr(SynExpr.Sequential _) :: _
                                  | SyntaxNode.SynExpr(SynExpr.ComputationExpr _) :: _
                                  | SyntaxNode.SynExpr(SynExpr.LetOrUse _) :: _ -> Some expr.Range
                                  | _ -> None

                              let waiter =
                                  if builder = "async" then
                                      "Async.Sleep"
                                  else
                                      "System.Threading.Tasks.Task.Delay"

                              target
                              |> Option.map (fun r ->
                                  r, textOfRange source r, $"do! {waiter} {argumentText source (stripParens arg)}")
                          | _ -> None

                      { Range = expr.Range
                        Kind = kind
                        Builder = Some builder
                        Fix = fix }
                  | None -> ()
              | None -> () ]
