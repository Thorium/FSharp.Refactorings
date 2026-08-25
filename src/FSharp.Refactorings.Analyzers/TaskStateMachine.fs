/// Refactoring advice for FS3511 ("this state machine is not statically
/// compilable"): oversized or `let rec`-carrying `task { }` bodies fall
/// back to the slow dynamic state-machine implementation at build time.
///
/// FS3511 itself is emitted during code generation, which the checker
/// never runs — no analyzer can observe the diagnostic. What IS statically
/// knowable:
///
///   - a `let rec` in the resumable body is a definite FS3511 producer
///   - very large bodies (many awaits, long span) are the at-risk shape
///
/// For flagged tasks the advice points at the shrinking moves:
///
///   a) plain `let`s before the first await add state-machine fields:
///      hoist them out before the builder
///   b) an if/match whose branches each await: give every branch its own
///      smaller `task { }` and pick between them outside
///   c) a long non-awaiting tail after the last await: extract it into a
///      plain function
///
/// The advice carries no automatic fix: moving code across the task
/// boundary changes when exceptions surface (a throw inside the builder
/// faults the Task; outside it throws synchronously), so the edit is the
/// author's call.
module FSharp.Refactorings.TaskStateMachine

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

[<RequireQualifiedAccess>]
type AdviceKind =
    /// A let rec sits in the resumable body — a definite FS3511.
    | HoistRecursiveFunction
    /// N plain lets before the first await can move out of the task.
    | HoistPlainLets of count: int
    /// Two or more branches await; each can be its own task.
    | SplitBranches
    /// N lines of non-awaiting code follow the last await.
    | ExtractTail of lineCount: int

type Suggestion = { Range: range; Kind: AdviceKind }

/// Builder names whose computation expressions compile to state machines.
let private taskBuilders = set [ "task"; "backgroundTask" ]

/// Awaits at or above this count mark a task as at risk of FS3511.
[<Literal>]
let private BangThreshold = 8

/// Body line spans at or above this mark a task as at risk of FS3511.
[<Literal>]
let private LineThreshold = 60

let private isBangExpr (e: SynExpr) =
    match e with
    | SynExpr.LetOrUse lou -> lou.IsBang
    | SynExpr.DoBang _
    | SynExpr.YieldOrReturnFrom _
    | SynExpr.MatchBang _ -> true
    | _ -> false

/// Leading non-bang lets of a CE body: their count, the first binding's
/// range, and the rest of the body.
[<TailCall>]
let rec private peelPlainLets (count: int) (firstRange: range option) (e: SynExpr) =
    match e with
    | SynExpr.LetOrUse lou when not (lou.IsBang || lou.IsUse) ->
        let firstRange =
            match firstRange, lou.Bindings with
            | None, binding :: _ -> Some binding.RangeOfBindingWithRhs
            | _ -> firstRange

        peelPlainLets (count + List.length lou.Bindings) firstRange lou.Body
    | _ -> count, firstRange, e

/// Advice for tasks that provably (let rec) or plausibly (size) hit FS3511.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    ignore source
    let index = AstIndex.ofTree parseTree

    let containsBang (r: range) =
        index.Exprs
        |> Array.exists (fun (_, e) -> isBangExpr e && Range.rangeContainsRange r e.Range)

    [ for _, expr in index.Exprs do
          match expr with
          | SynExpr.App(isInfix = false; funcExpr = IdentName builder; argExpr = SynExpr.ComputationExpr(expr = body)) when
              taskBuilders.Contains builder
              ->

              // sub-ranges whose contents are not this task's resumable code
              let opaqueRanges =
                  index.Exprs
                  |> Array.choose (fun (_, e) ->
                      match e with
                      | SynExpr.Lambda _
                      | SynExpr.ComputationExpr _ when Range.rangeContainsRange body.Range e.Range -> Some e.Range
                      | _ -> None)

              let inResumableBody (r: range) =
                  Range.rangeContainsRange body.Range r
                  && not (opaqueRanges |> Array.exists (fun o -> Range.rangeContainsRange o r))

              let recursiveLets =
                  index.Exprs
                  |> Array.filter (fun (_, e) ->
                      match e with
                      | SynExpr.LetOrUse lou -> lou.IsRecursive && not lou.IsBang && inResumableBody e.Range
                      | _ -> false)

              let bangCount =
                  index.Exprs
                  |> Array.filter (fun (_, e) -> isBangExpr e && inResumableBody e.Range)
                  |> Array.length

              let bodyLines = body.Range.EndLine - body.Range.StartLine + 1

              for _, letRec in recursiveLets do
                  match letRec with
                  | SynExpr.LetOrUse lou ->
                      match lou.Bindings with
                      | binding :: _ ->
                          { Range = binding.RangeOfBindingWithRhs
                            Kind = AdviceKind.HoistRecursiveFunction }
                      | [] -> ()
                  | _ -> ()

              // the shrink advice only for genuinely oversized tasks
              if bangCount >= BangThreshold || bodyLines >= LineThreshold then

                  // a) leading plain lets
                  let letCount, firstLetRange, rest = peelPlainLets 0 None body

                  match firstLetRange with
                  | Some r when letCount > 0 ->
                      { Range = r
                        Kind = AdviceKind.HoistPlainLets letCount }
                  | _ -> ()

                  // b) branching where several arms await
                  match rest with
                  | SynExpr.IfThenElse(thenExpr = thenExpr; elseExpr = Some elseExpr) when
                      containsBang thenExpr.Range && containsBang elseExpr.Range
                      ->
                      { Range = rest.Range
                        Kind = AdviceKind.SplitBranches }
                  | SynExpr.Match(clauses = clauses) when
                      (clauses
                       |> List.filter (fun (SynMatchClause(resultExpr = result)) -> containsBang result.Range)
                       |> List.length)
                      >= 2
                      ->
                      { Range = rest.Range
                        Kind = AdviceKind.SplitBranches }
                  | _ -> ()

                  // c) a long non-awaiting tail after the last await
                  let lastBangLine =
                      index.Exprs
                      |> Array.fold
                          (fun acc (_, e) ->
                              if isBangExpr e && inResumableBody e.Range then
                                  max acc e.Range.StartLine
                              else
                                  acc)
                          0

                  if lastBangLine > 0 then
                      let tailLines = body.Range.EndLine - lastBangLine

                      if tailLines >= 4 then
                          { Range =
                              Range.mkRange body.Range.FileName (Position.mkPos (lastBangLine + 1) 0) body.Range.End
                            Kind = AdviceKind.ExtractTail tailLines }
          | _ -> () ]
