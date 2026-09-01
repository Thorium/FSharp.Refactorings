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

open System.Text.RegularExpressions
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
        /// (range, original, replacement) edits: Thread.Sleep in statement
        /// position, or a GetResult binding becoming a let! bind. These
        /// move code TOWARD async and auto-apply.
        Fixes: (range * string * string) list
        /// The sync-sibling swap for a boundary GetResult — offered in
        /// editors and behind the `"FR0049": { "syncSwap": 1 }` config
        /// knob only, never auto-applied: async-in-sync is usually a
        /// waypoint toward a full-async refactor, and swapping to the
        /// sync API walks the code the other way.
        AlternativeFixes: (range * string * string) list
        /// For a BOUNDARY site (Builder = None): the task-typed receiver
        /// being drained, when the shape exposes one — `t.Result` gives
        /// `t`, `t.GetAwaiter().GetResult()` gives `t`. The taskify fix
        /// binds this with let!/return!.
        Receiver: range voption
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

/// `RECV.GetAwaiter()` — the expression whose awaiter is being drained.
[<return: Struct>]
let private (|AwaiterReceiver|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(
        isInfix = false
        funcExpr = SynExpr.DotGet(expr = recv; longDotId = SynLongIdent(id = [ aw ]))
        argExpr = UnitConst) when aw.idText = "GetAwaiter" -> ValueSome recv
    | _ -> ValueNone

/// The range of a dotted path minus its last segment: `t.tail.Result`
/// gives `t.tail`.
let private prefixRangeOf (e: SynExpr) (ids: Ident list) =
    let prefix = ids |> List.take (ids.Length - 1)
    Range.mkRange e.Range.FileName (List.head prefix).idRange.Start (List.last prefix).idRange.End

/// The receiver's source range, covering both parse shapes: a DotGet on a
/// call result, and the flat LongIdent path `t.GetAwaiter` a simple
/// identifier receiver parses to.
[<return: Struct>]
let private (|AwaiterReceiverRange|_|) (e: SynExpr) =
    match e with
    | AwaiterReceiver recv -> ValueSome recv.Range
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = UnitConst) when
        ids.Length >= 2 && (List.last ids).idText = "GetAwaiter"
        ->
        ValueSome(prefixRangeOf e ids)
    | _ -> ValueNone

/// A full `Async.RunSynchronously` application whose only argument is the
/// computation: the pipe form, or direct application of a single plain
/// argument. A tuple argument carries timeout/cancellation and cannot
/// become a bind.
[<return: Struct>]
let private (|RunSyncApplication|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(
        funcExpr = SynExpr.App(isInfix = true; funcExpr = pipeOp; argExpr = comp)
        argExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))) when
        (match pipeOp with
         | SynExpr.Ident op -> op.idText = "op_PipeRight"
         | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ op ])) -> op.idText = "op_PipeRight"
         | _ -> false)
        && pathEndsWith "Async" "RunSynchronously" ids
        ->
        ValueSome comp
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = comp) when
        pathEndsWith "Async" "RunSynchronously" ids
        && (match stripParens comp with
            | SynExpr.Tuple _ -> false
            | _ -> true)
        ->
        ValueSome comp
    | _ -> ValueNone

/// Wrap an expression's text in parentheses unless it is a bare
/// identifier path — `Async.AwaitTask client.GetAsync(u)` would apply to
/// the wrong thing.
let private asArgument (text: string) =
    if Regex.IsMatch(text, @"^[A-Za-z_][\w'.]*$") then
        text
    else
        $"({text})"

/// Is a type Task/ValueTask/Async — i.e. still asynchronous?
let private isAwaitableType (t: FSharpType) =
    try
        match t.StripAbbreviations().TypeDefinition.TryFullName with
        | Some full ->
            full.StartsWith "System.Threading.Tasks.Task"
            || full.StartsWith "System.Threading.Tasks.ValueTask"
            || full.StartsWith "Microsoft.FSharp.Control.FSharpAsync"
        | None -> false
    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
        false

/// A boundary `RECV.SomethingAsync(args).GetAwaiter().GetResult()` whose
/// declaring entity provably offers a synchronous `Something` with the
/// same argument count: the call swaps to the sibling and the awaiter
/// chain drops. Verified against the typed tree, never guessed from the
/// name alone.
let private syncSiblingFix
    (check: FSharpCheckFileResults)
    (source: ISourceText)
    (recv: SynExpr)
    (whole: SynExpr)
    : (range * string * string) list =
    let callIdent =
        match recv with
        | SynExpr.App(isInfix = false; funcExpr = SynExpr.DotGet(longDotId = SynLongIdent(id = ids)); argExpr = arg)
        | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = arg) when
            not ids.IsEmpty
            ->
            Some(List.last ids, arg)
        | _ -> None

    match callIdent with
    | Some(id, arg) when id.idText.EndsWith "Async" && id.idText.Length > "Async".Length ->
        let trimmed = id.idText.Substring(0, id.idText.Length - "Async".Length)

        let argCount =
            match stripParens arg with
            | SynExpr.Const(SynConst.Unit, _) -> 0
            | SynExpr.Tuple(exprs = es) -> es.Length
            | _ -> 1

        let r = id.idRange
        let lineText = source.GetLineString(r.EndLine - 1)

        let hasSibling =
            match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ id.idText ]) with
            | Some symbolUse ->
                match symbolUse.Symbol with
                | :? FSharpMemberOrFunctionOrValue as mfv ->
                    match mfv.DeclaringEntity with
                    | Some entity ->
                        entity.MembersFunctionsAndValues
                        |> Seq.exists (fun m ->
                            m.DisplayName = trimmed
                            // a PROPERTY named like the sibling would turn
                            // `x.Foo(args)` into applying unit to a value
                            && not m.IsProperty
                            && not m.IsPropertyGetterMethod
                            && (m.CurriedParameterGroups |> Seq.sumBy Seq.length) = argCount
                            && not (isAwaitableType m.ReturnParameter.Type))
                    | None -> false
                | _ -> false
            | None -> false

        if hasSibling then
            let dropRange = Range.mkRange whole.Range.FileName recv.Range.End whole.Range.End

            [ id.idRange, id.idText, trimmed; dropRange, textOfRange source dropRange, "" ]
        else
            []
    | _ -> []

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

        // `let!`/`do!` cannot appear inside a finally block or an exception
        // handler — no bind-shaped fix may land in one
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

        let inNoBindZone (r: range) =
            noBindRanges |> Array.exists (fun z -> Range.rangeContainsRange z r)

        [ for path, expr in index.Exprs do
              let blocking =
                  match expr with
                  // t.Result — a bare property path or a dot-get
                  | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
                      ids.Length >= 2 && (List.last ids).idText = "Result"
                      ->
                      let id = List.last ids

                      if (enclosingEntityOf check source id) |> taskFamily then
                          Some(BlockKind.TaskResult, None, Some id)
                      else
                          None
                  | SynExpr.DotGet(longDotId = SynLongIdent(id = [ id ])) when
                      id.idText = "Result" && (enclosingEntityOf check source id) |> taskFamily
                      ->
                      Some(BlockKind.TaskResult, None, Some id)
                  | SynExpr.App(isInfix = false; funcExpr = CallIdent id) when
                      (id.idText = "Wait" || id.idText = "WaitAll" || id.idText = "WaitAny")
                      && (enclosingEntityOf check source id) |> taskFamily
                      ->
                      Some(BlockKind.TaskWait, None, Some id)
                  | SynExpr.App(isInfix = false; funcExpr = CallIdent id; argExpr = UnitConst) when
                      id.idText = "GetResult"
                      && (enclosingEntityOf check source id).Contains "Awaiter"
                      ->
                      Some(BlockKind.AwaiterGetResult, None, Some id)
                  | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
                      pathEndsWith "Async" "RunSynchronously" ids
                      && (fullNameOf check source (List.last ids)).StartsWith "Microsoft.FSharp.Control"
                      ->
                      Some(BlockKind.RunSynchronously, None, Some(List.last ids))
                  | SynExpr.App(
                      isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = arg) when
                      pathEndsWith "Thread" "Sleep" ids
                      && (enclosingEntityOf check source (List.last ids)) = "System.Threading.Thread"
                      ->
                      Some(BlockKind.ThreadSleep, Some arg, None)
                  | _ -> None

              match blocking with
              | Some(kind, sleepArg, blockIdent) ->
                  match innermostCe expr.Range with
                  | None when kind <> BlockKind.ThreadSleep && kind <> BlockKind.RunSynchronously ->
                      // sync-over-async at a boundary: an antipattern even
                      // outside CEs — either the caller becomes async (wrap
                      // in task { } and bind) or the synchronous API should
                      // be used. Thread.Sleep in sync code is legitimate,
                      // and Async.RunSynchronously outside a CE IS F#'s
                      // intended sync-boundary runner.
                      let alternatives =
                          match kind, expr with
                          | BlockKind.AwaiterGetResult,
                            SynExpr.App(funcExpr = SynExpr.DotGet(expr = AwaiterReceiver recv)) ->
                              syncSiblingFix check source recv expr
                          | _ -> []

                      let receiver =
                          match kind, expr with
                          | BlockKind.AwaiterGetResult,
                            SynExpr.App(funcExpr = SynExpr.DotGet(expr = AwaiterReceiverRange recvRange)) ->
                              ValueSome recvRange
                          | BlockKind.TaskResult, SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
                              ids.Length >= 2
                              ->
                              ValueSome(prefixRangeOf expr ids)
                          | BlockKind.TaskResult, SynExpr.DotGet(expr = recv) -> ValueSome recv.Range
                          | _ -> ValueNone

                      { Range = expr.Range
                        Kind = kind
                        Builder = None
                        Fixes = []
                        AlternativeFixes = alternatives
                        Receiver = receiver }
                  | Some(builder, ceRange) ->
                      let fixes =
                          match kind, sleepArg with
                          | BlockKind.ThreadSleep, Some arg when
                              not (insideLambdaWithin ceRange expr.Range)
                              && not (insideOtherCeWithin ceRange expr.Range)
                              && not (inNoBindZone expr.Range)
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
                              |> Option.toList
                          | (BlockKind.AwaiterGetResult | BlockKind.TaskResult | BlockKind.RunSynchronously), _ when
                              not (insideLambdaWithin ceRange expr.Range)
                              && not (insideOtherCeWithin ceRange expr.Range)
                              && not (inNoBindZone expr.Range)
                              ->
                              // `let x = <blocking>` as a direct CE statement
                              // becomes `let! x = <computation>` — the
                              // builder's own bind releases the thread. The
                              // adapter matrix is asymmetric: task { } binds
                              // both Tasks and Asyncs with a plain let!,
                              // async { } binds Asyncs natively but needs
                              // Async.AwaitTask for a Task — and ValueTask
                              // has no AwaitTask overload at all, so those
                              // stay advice in async
                              let taskBuilder = builder = "task" || builder = "backgroundTask"

                              // the plain `let` whose entire RHS is `target`
                              // — found structurally, not via the walker's
                              // path conventions
                              let bindingKeywordFor (target: range) =
                                  index.Exprs
                                  |> Array.tryPick (fun (_, e) ->
                                      match e with
                                      | LetOrUseE lou when not (lou.IsBang || lou.IsUse || lou.IsRecursive) ->
                                          match lou.Bindings with
                                          // simple named pattern, no type
                                          // annotation: `let! x : T = ..` is
                                          // not a shape to gamble on
                                          | [ SynBinding(
                                                  isMutable = false
                                                  returnInfo = None
                                                  headPat = SynPat.Named _
                                                  expr = rhs
                                                  trivia = btrivia) ] when Range.equals rhs.Range target ->
                                              Some btrivia.LeadingKeyword.Range
                                          | _ -> None
                                      | _ -> None)

                              let bindingRewrite (target: range) (bound: string) =
                                  match bindingKeywordFor target with
                                  | Some kw when textOfRange source kw = "let" ->
                                      [ kw, "let", "let!"; target, textOfRange source target, bound ]
                                  | _ -> []

                              // a TASK receiver: direct in task { }, behind
                              // Async.AwaitTask in async { } (real Task only)
                              let bindTaskReceiver (recvRange: range) =
                                  let text = textOfRange source recvRange

                                  if taskBuilder then
                                      Some text
                                  elif builder = "async" then
                                      let entity =
                                          blockIdent
                                          |> Option.map (enclosingEntityOf check source)
                                          |> Option.defaultValue ""

                                      if
                                          entity.StartsWith "System.Threading.Tasks.Task"
                                          || entity.StartsWith "System.Runtime.CompilerServices.TaskAwaiter"
                                      then
                                          Some $"Async.AwaitTask {asArgument text}"
                                      else
                                          None
                                  else
                                      None

                              match kind, expr with
                              | BlockKind.AwaiterGetResult,
                                SynExpr.App(funcExpr = SynExpr.DotGet(expr = AwaiterReceiverRange recvRange)) ->
                                  bindTaskReceiver recvRange
                                  |> Option.map (bindingRewrite expr.Range)
                                  |> Option.defaultValue []
                              | BlockKind.TaskResult, SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
                                  ids.Length >= 2
                                  ->
                                  bindTaskReceiver (prefixRangeOf expr ids)
                                  |> Option.map (bindingRewrite expr.Range)
                                  |> Option.defaultValue []
                              | BlockKind.TaskResult, SynExpr.DotGet(expr = recv) ->
                                  bindTaskReceiver recv.Range
                                  |> Option.map (bindingRewrite expr.Range)
                                  |> Option.defaultValue []
                              | BlockKind.RunSynchronously, _ ->
                                  // the flagged node is the ident; the
                                  // binding RHS is the surrounding
                                  // application — an Async binds natively in
                                  // BOTH builders
                                  index.Exprs
                                  |> Array.tryPick (fun (_, e) ->
                                      match e with
                                      | RunSyncApplication comp when Range.rangeContainsRange e.Range expr.Range ->
                                          Some(e.Range, comp)
                                      | _ -> None)
                                  |> Option.map (fun (rhsRange, comp) ->
                                      bindingRewrite rhsRange (textOfRange source comp.Range))
                                  |> Option.defaultValue []
                              | _ -> []
                          | _ -> []

                      { Range = expr.Range
                        Kind = kind
                        Builder = Some builder
                        Fixes = fixes
                        AlternativeFixes = []
                        Receiver = ValueNone }
                  | None -> ()
              | None -> () ]
