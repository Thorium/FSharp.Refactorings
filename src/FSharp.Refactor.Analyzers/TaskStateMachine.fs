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
/// Three of the moves now carry automatic fixes, each shaped so the moved
/// text stays verbatim wherever possible:
///
///   a) leading plain lets hoist ABOVE the builder line (dedented to its
///      column). Caveat: a throw in hoisted code now surfaces at the call
///      instead of faulting the returned Task — the same trade the advice
///      always asked for.
///   b) the non-awaiting tail wraps into a LOCAL function defined inside
///      the CE and called as its last statement. A nested function's body
///      is not resumable code (this rule itself treats lambdas as opaque),
///      so the state machine shrinks — and because the function stays in
///      scope, closures capture every CE local: no parameters, no type
///      annotations, no inference risk.
///   c) a body that IS an if/else whose both arms await splits into
///      `if c then task { .. } else task { .. }` — arm text verbatim.
///      With leading lets present, (a) goes first and the multi-pass loop
///      brings (c) around on the next pass.
module FSharp.Refactor.TaskStateMachine

open System.Text.RegularExpressions
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

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
    /// The task's closing block awaits in shapes the tail wrap cannot
    /// carry (early returns, try/finally around the awaits); it can be a
    /// task-returning local function of its own, consumed with return!.
    | ExtractAwaitingSuffix of lineCount: int

type Suggestion =
    {
        Range: range
        Kind: AdviceKind
        /// (range, replacement) pairs when the advice carries an automatic
        /// fix; empty when the edit stays the author's call.
        Edits: (range * string) list
    }

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

/// A binding a hoist can move: no attributes, not mutable (a closure in
/// the remaining body could not capture it once hoisted), not inline.
let private hoistable (binding: SynBinding) =
    match binding with
    | SynBinding(attributes = []; isMutable = false; isInline = false) -> true
    | _ -> false

/// Leading non-bang lets of a CE body: their count, the first binding's
/// range, and the rest of the body. Stops at the first binding a hoist
/// could not carry, so the count is exactly what the fix can move.
[<TailCall>]
let rec private peelPlainLets (count: int) (firstRange: range option) (e: SynExpr) =
    match e with
    | SynExpr.LetOrUse lou when
        not (lou.IsBang || lou.IsUse || lou.IsRecursive)
        && lou.Bindings |> List.forall hoistable
        ->
        let firstRange =
            match firstRange, lou.Bindings with
            | None, binding :: _ -> Some binding.RangeOfBindingWithRhs
            | _ -> firstRange

        peelPlainLets (count + List.length lou.Bindings) firstRange lou.Body
    | _ -> count, firstRange, e

/// Only whitespace sits left of the range on its start line.
let private startsOwnLine (source: ISourceText) (r: range) =
    r.StartColumn = 0
    || (source.GetLineString(r.StartLine - 1)).Substring(0, r.StartColumn).Trim() = ""

let private leadingSpaces (line: string) =
    line.Length - line.TrimStart(' ').Length

let private isBlank (line: string) = line.Trim() = ""

/// Lines of the file from `startLine` to `endLine` inclusive (1-based).
let private linesOf (source: ISourceText) (startLine: int) (endLine: int) =
    [ for l in startLine..endLine -> source.GetLineString(l - 1) ]

/// Re-indenting moved text is only safe when no line's leading whitespace
/// belongs to a string literal: multi-line strings travel verbatim-only.
let private multiLineStringSafe (lines: string list) =
    lines
    |> List.forall (fun l -> not ((l.Contains "\"\"\"") || (l.Contains "@\"")))

/// The textual probe above misses PLAIN literals spanning lines ("line1
/// <newline> line2" is legal F#) — the AST sees them exactly.
let private spansMultiLineLiteral (index: AstIndex.Index) (startLine: int) (endLine: int) =
    index.Exprs
    |> Array.exists (fun (_, e) ->
        (match e with
         | SynExpr.Const(SynConst.String _, _)
         | SynExpr.InterpolatedString _ -> true
         | _ -> false)
        && e.Range.StartLine < e.Range.EndLine
        && e.Range.StartLine <= endLine
        && e.Range.EndLine >= startLine)

/// Shift every non-blank line left by `n` columns; None when any line has
/// less indentation than that.
let private dedentBy (n: int) (lines: string list) =
    if n = 0 then
        Some lines
    elif lines |> List.forall (fun l -> isBlank l || leadingSpaces l >= n) then
        Some(lines |> List.map (fun l -> if isBlank l then "" else l.Substring n))
    else
        None

/// Extend a moved region's start upward over the comment block that
/// documents it: contiguous `//`/`///` lines, crossing blank lines only
/// when another comment line sits above them. Doc comments travel with
/// the code they describe; stray blank lines above the block stay put.
let private extendUpOverComments (source: ISourceText) (floorLine: int) (startLine: int) =
    let line n = source.GetLineString(n - 1)
    let isComment (l: string) = l.TrimStart().StartsWith "//"

    let mutable top = startLine
    let mutable probe = startLine - 1

    while probe > floorLine && (isComment (line probe) || isBlank (line probe)) do
        if isComment (line probe) then
            top <- probe

        probe <- probe - 1

    top

/// The terminal expression of a CE statement chain.
[<TailCall>]
let rec private terminalOf (e: SynExpr) =
    match e with
    | SynExpr.LetOrUse lou when not lou.IsBang -> terminalOf lou.Body
    | SynExpr.Sequential(expr2 = b) -> terminalOf b
    | t -> t

/// A fresh function name for extracted code: the base name, or a numbered
/// variant when the file already uses that identifier.
/// The tail wrap's own output: a single nullary local function immediately
/// called (or returned). Wrapping THAT again — runTail2 around runTail,
/// runTail3 around runTail2 — sheds nothing from the state machine and
/// never converges; seen live three layers deep on management-portal.
let private alreadyWrappedTail (tail: SynExpr) =
    match tail with
    | SynExpr.LetOrUse lou when not lou.IsBang ->
        match lou.Bindings, lou.Body with
        | [ SynBinding(headPat = SynPat.LongIdent(longDotId = SynLongIdent(id = [ f ]); argPats = SynArgPats.Pats [ _ ])) ],
          (SynExpr.App(funcExpr = SynExpr.Ident g)
          | SynExpr.YieldOrReturn(expr = SynExpr.App(funcExpr = SynExpr.Ident g))
          | SynExpr.YieldOrReturnFrom(expr = SynExpr.App(funcExpr = SynExpr.Ident g))) -> f.idText = g.idText
        | _ -> false
    | _ -> false

let private freshName (source: ISourceText) (baseName: string) =
    let full =
        String.concat "\n" [ for i in 0 .. source.GetLineCount() - 1 -> source.GetLineString i ]

    [ baseName; baseName + "2"; baseName + "3" ]
    |> List.tryFind (fun candidate -> not (Regex.IsMatch(full, identifierPattern candidate)))

/// Advice for tasks that provably (let rec) or plausibly (size) hit FS3511.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    let containsBang (r: range) =
        index.Exprs
        |> Array.exists (fun (_, e) -> isBangExpr e && Range.rangeContainsRange r e.Range)

    // the suffix of a CE's top-level statement chain that follows its last
    // awaiting step; None when no step awaits
    let rec tailAfterLastBang (e: SynExpr) : SynExpr option =
        match e with
        | SynExpr.LetOrUse lou when not lou.IsBang ->
            match tailAfterLastBang lou.Body with
            | Some t -> Some t
            | None ->
                if lou.Bindings |> List.exists (fun b -> containsBang b.RangeOfBindingWithRhs) then
                    Some lou.Body
                else
                    None
        | SynExpr.LetOrUse lou -> // a let!/use! step: what follows is its body
            match tailAfterLastBang lou.Body with
            | Some t -> Some t
            | None -> Some lou.Body
        | SynExpr.Sequential(expr1 = a; expr2 = b) ->
            match tailAfterLastBang b with
            | Some t -> Some t
            | None ->
                if isBangExpr a || containsBang a.Range then
                    Some b
                else
                    None
        | _ -> None

    // LOCAL mutable bindings anywhere in the file, with where they are
    // declared: a closure cannot capture one (read or write), so a block
    // becoming a local function must not mention any declared OUTSIDE
    // itself — its own mutables move with it and stay legal. Module-level
    // mutables are static fields and capture fine, but they are
    // declarations, not exprs, so they never land in this set — the
    // over-approximation is only that a same-named local in another
    // function also blocks
    let localMutables =
        index.Exprs
        |> Array.collect (fun (_, e) ->
            match e with
            | SynExpr.LetOrUse lou when not lou.IsBang ->
                lou.Bindings
                |> List.choose (fun b ->
                    match b with
                    | SynBinding(isMutable = true; headPat = SynPat.Named(ident = SynIdent(ident = id))) ->
                        Some(id.idText, b.RangeOfBindingWithRhs)
                    | _ -> None)
                |> Array.ofList
            | _ -> [||])

    let mentionsForeignMutable (blockRange: range) (text: string) =
        localMutables
        |> Array.exists (fun (name, declRange) ->
            not (Range.rangeContainsRange blockRange declRange)
            && Regex.IsMatch(text, identifierPattern name))

    // every identifier a pattern binds; None when the pattern has a shape
    // this walk does not understand (then nothing may rely on the answer)
    let rec patIdents (p: SynPat) : string list option =
        match p with
        | SynPat.Named(ident = SynIdent(ident = id)) -> Some [ id.idText ]
        | SynPat.Wild _ -> Some []
        | SynPat.Typed(pat = inner) -> patIdents inner
        | SynPat.Paren(pat = inner) -> patIdents inner
        | SynPat.Tuple(elementPats = els) ->
            els
            |> List.map patIdents
            |> List.fold
                (fun acc cur ->
                    match acc, cur with
                    | Some a, Some c -> Some(a @ c)
                    | _ -> None)
                (Some [])
        | SynPat.LongIdent(longDotId = SynLongIdent(id = [ id ])) -> Some [ id.idText ]
        | _ -> None

    [ for _, expr in index.Exprs do
          match expr with
          | SynExpr.App(
              isInfix = false; funcExpr = fe & IdentName builder; argExpr = SynExpr.ComputationExpr(expr = body)) when
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
                            Kind = AdviceKind.HoistRecursiveFunction
                            Edits = [] }
                      | [] -> ()
                  | _ -> ()

              // the shrink advice only for genuinely oversized tasks
              if bangCount >= BangThreshold || bodyLines >= LineThreshold then
                  let fileName = body.Range.FileName
                  let taskIndentText = String.replicate fe.Range.StartColumn " "

                  // a) leading plain lets — the fix hoists their lines above
                  // the builder, dedented to its column. A throw in hoisted
                  // code surfaces at the call instead of faulting the Task;
                  // that trade is the advice itself
                  let letCount, firstLetRange, rest = peelPlainLets 0 None body

                  match firstLetRange with
                  | Some r when letCount > 0 ->
                      let hoistEdits =
                          // the binding's documenting comment block (blank
                          // lines between comment runs included) moves too
                          let startLine = extendUpOverComments source fe.Range.StartLine r.StartLine
                          let endLineExcl = rest.Range.StartLine

                          if
                              startsOwnLine source fe.Range
                              && startLine > fe.Range.StartLine
                              && endLineExcl > startLine
                              // the last moved binding must not spill onto
                              // the rest's line: whole lines move or nothing
                              && startsOwnLine source rest.Range
                          then
                              let movedLines = linesOf source startLine (endLineExcl - 1)

                              // the region's first CODE line must be the let
                              // itself and sets the dedent: FCS includes ///
                              // doc comments in the binding's range, and the
                              // extension above adds plain // blocks
                              let letLine =
                                  movedLines
                                  |> List.tryFind (fun l -> not (isBlank l || l.TrimStart().StartsWith "//"))
                                  |> Option.defaultValue ""

                              let movedRange =
                                  Range.mkRange fileName (Position.mkPos startLine 0) (Position.mkPos endLineExcl 0)

                              match dedentBy (leadingSpaces letLine - fe.Range.StartColumn) movedLines with
                              | Some dedented when
                                  letLine.TrimStart().StartsWith "let "
                                  && multiLineStringSafe movedLines
                                  && not (spansMultiLineLiteral index startLine (endLineExcl - 1))
                                  && not (spansDirective source movedRange)
                                  ->
                                  [ Range.mkRange fileName fe.Range.Start fe.Range.Start,
                                    (String.concat "\n" dedented).TrimStart() + "\n" + taskIndentText
                                    movedRange, "" ]
                              | _ -> []
                          else
                              []

                      { Range = r
                        Kind = AdviceKind.HoistPlainLets letCount
                        Edits = hoistEdits }
                  | _ -> ()

                  // b) branching — when the body IS the if (letCount 0; pass
                  // order lets (a) clear the lets first), the fix splits it
                  // into a task per arm, arm text verbatim. One awaiting arm
                  // is enough: a synchronous arm becomes a trivially static
                  // `task { return .. }`, and the big arm gets its own
                  // smaller machine
                  match rest with
                  | SynExpr.IfThenElse(ifExpr = cond; thenExpr = thenExpr; elseExpr = Some elseExpr; trivia = trivia) when
                      containsBang thenExpr.Range || containsBang elseExpr.Range
                      ->
                      let splitEdits =
                          let lineTailBlank (r: range) =
                              (source.GetLineString(r.EndLine - 1)).Substring(r.EndColumn).Trim() = ""

                          // `task { .. } |> f` or `.ContinueWith ..` binds
                          // tighter than a bare if/else would: leave those
                          let noContinuationAfter =
                              lineTailBlank expr.Range
                              && (seq { expr.Range.EndLine + 1 .. source.GetLineCount() }
                                  |> Seq.map (fun l -> (source.GetLineString(l - 1)).Trim())
                                  |> Seq.tryFind (fun t -> t <> "")
                                  |> Option.forall (fun t ->
                                      not (t.StartsWith '|' || t.StartsWith '.' || t.StartsWith ":>")))

                          // the arms are cut as LINE regions between four
                          // anchors — the `if .. then` header, the `else`
                          // keyword line, and the CE's closing brace line —
                          // so every comment line in the replaced span lands
                          // in one arm or the other by construction (a
                          // comment above an arm would otherwise sit outside
                          // the arm expression's range and be dropped, and
                          // the comment guard would hold the whole fix back)
                          let elseKwLine =
                              match trivia.ElseKeyword with
                              | Some ek when (source.GetLineString(ek.StartLine - 1)).Trim() = "else" ->
                                  Some ek.StartLine
                              | _ -> None

                          let ifLine = rest.Range.StartLine
                          let closeLine = expr.Range.EndLine

                          match elseExpr, elseKwLine with
                          | SynExpr.IfThenElse _, _ -> [] // elif chains stay advice
                          | _, Some elseKwLine when
                              letCount = 0
                              && startsOwnLine source fe.Range
                              // the if directly follows `task {`: no line of
                              // the replaced span sits outside the arms
                              && ifLine = fe.Range.StartLine + 1
                              && isSingleLine cond.Range
                              && (source.GetLineString(cond.Range.EndLine - 1)).TrimEnd().EndsWith "then"
                              && thenExpr.Range.StartLine > cond.Range.EndLine
                              && elseKwLine > thenExpr.Range.EndLine
                              && elseExpr.Range.StartLine > elseKwLine
                              && startsOwnLine source thenExpr.Range
                              && startsOwnLine source elseExpr.Range
                              && lineTailBlank thenExpr.Range
                              && lineTailBlank elseExpr.Range
                              && (source.GetLineString(closeLine - 1)).Trim() = "}"
                              && elseExpr.Range.EndLine < closeLine
                              && noContinuationAfter
                              && not (spansDirective source expr.Range)
                              ->
                              // arms re-home one level under their new task;
                              // verbatim when a dedent would not be safe
                              let armText (startLine: int) (endLine: int) =
                                  let lines = linesOf source startLine endLine

                                  let indent =
                                      lines
                                      |> List.filter (isBlank >> not)
                                      |> List.map leadingSpaces
                                      |> List.fold min System.Int32.MaxValue

                                  let shift = indent - (fe.Range.StartColumn + 4)

                                  match (if shift > 0 then dedentBy shift lines else None) with
                                  | Some d when multiLineStringSafe lines -> String.concat "\n" d
                                  | _ -> String.concat "\n" lines

                              // the arms keep the ORIGINAL builder — a
                              // backgroundTask split into plain tasks would
                              // silently lose its thread-pool start
                              [ Range.mkRange fileName (Position.mkPos fe.Range.StartLine 0) expr.Range.End,
                                taskIndentText
                                + "if "
                                + textOfRange source cond.Range
                                + $" then {builder} {{\n"
                                + armText (ifLine + 1) (elseKwLine - 1)
                                + "\n"
                                + taskIndentText
                                + $"}} else {builder} {{\n"
                                + armText (elseKwLine + 1) (closeLine - 1)
                                + "\n"
                                + taskIndentText
                                + "}" ]
                          | _ -> []

                      { Range = rest.Range
                        Kind = AdviceKind.SplitBranches
                        Edits = splitEdits }
                  | SynExpr.Match(clauses = clauses)
                  | SynExpr.MatchBang(clauses = clauses) when
                      (clauses
                       |> List.filter (fun (SynMatchClause(resultExpr = result)) -> containsBang result.Range)
                       |> List.length)
                      >= 2
                      ->
                      // the match header never moves (match! NEEDS the outer
                      // bind), so the split happens per ARM: each awaiting
                      // arm's body becomes `return! task { .. }` — a nested
                      // machine that carries the arm's weight, returns still
                      // legal inside. All-or-nothing across awaiting arms.
                      let armEdits =
                          let perArm =
                              [ for SynMatchClause(resultExpr = result) as clause in clauses do
                                    // a single return statement is the whole
                                    // arm: wrapping it moves NOTHING out of
                                    // the machine, and the wrapped result is
                                    // itself a single return! — the shape
                                    // that once re-wrapped every pass into
                                    // return! task { return! task { ... } }
                                    let singleReturnArm =
                                        match result with
                                        | SynExpr.YieldOrReturn _
                                        | SynExpr.YieldOrReturnFrom _ -> true
                                        | _ -> false

                                    if containsBang result.Range && not singleReturnArm then
                                        let r = result.Range
                                        let bodyLines = linesOf source r.StartLine r.EndLine
                                        let armInd = String.replicate r.StartColumn " "

                                        if
                                            startsOwnLine source r
                                            && r.StartLine > clause.Range.StartLine
                                            && (source.GetLineString(r.EndLine - 1)).Substring(r.EndColumn).Trim() = ""
                                            && multiLineStringSafe bodyLines
                                            && not (spansMultiLineLiteral index r.StartLine r.EndLine)
                                            && not (spansDirective source r)
                                            && not (
                                                mentionsForeignMutable r (String.concat "\n" bodyLines)
                                            )
                                        then
                                            Some
                                                [ // opening rides the first line's indent insert
                                                  for l in r.StartLine .. r.EndLine do
                                                      let text = source.GetLineString(l - 1)

                                                      let opening =
                                                          if l = r.StartLine then
                                                              $"{armInd}return! {builder} {{\n"
                                                          else
                                                              ""

                                                      if l = r.StartLine || not (isBlank text) then
                                                          Range.mkRange fileName (Position.mkPos l 0) (Position.mkPos l 0),
                                                          opening + (if isBlank text then "" else "    ")
                                                  Range.mkRange fileName r.End r.End, $"\n{armInd}}}" ]
                                        else
                                            None ]

                          if perArm |> List.forall Option.isSome then
                              perArm |> List.collect Option.get
                          else
                              []

                      { Range = rest.Range
                        Kind = AdviceKind.SplitBranches
                        Edits = armEdits }
                  | _ -> ()

                  // c) a long non-awaiting tail after the last await — the
                  // fix wraps it in a LOCAL function inside the CE (a nested
                  // function's body is not resumable code) and calls it as
                  // the last statement; closures capture every CE local, so
                  // no parameters and no type annotations
                  let lastBangLine =
                      index.Exprs
                      |> Array.fold
                          (fun acc (_, e) ->
                              if isBangExpr e && inResumableBody e.Range then
                                  max acc e.Range.StartLine
                              else
                                  acc)
                          0

                  let mutable tailFixOffered = false

                  if lastBangLine > 0 then
                      // local function definitions in the tail compile to
                      // closures, not resumable code — they weigh nothing,
                      // and NOT counting them is what makes the extraction
                      // converge instead of re-wrapping its own output
                      let functionDefLines (r: range) =
                          index.Exprs
                          |> Array.sumBy (fun (_, e) ->
                              match e with
                              | SynExpr.LetOrUse lou when not lou.IsBang && Range.rangeContainsRange r e.Range ->
                                  lou.Bindings
                                  |> List.sumBy (fun b ->
                                      match b with
                                      | SynBinding(headPat = SynPat.LongIdent(argPats = SynArgPats.Pats(_ :: _))) ->
                                          let br = b.RangeOfBindingWithRhs
                                          br.EndLine - br.StartLine + 1
                                      | _ -> 0)
                              | _ -> 0)

                      let noteRange =
                          Range.mkRange fileName (Position.mkPos (lastBangLine + 1) 0) body.Range.End

                      let tailLineCount = body.Range.EndLine - lastBangLine - functionDefLines noteRange

                      if tailLineCount >= 4 then
                          let tailEdits =
                              match tailAfterLastBang body, freshName source "runTail" with
                              | Some tail, Some fnName when
                                  not (containsBang tail.Range)
                                  && startsOwnLine source tail.Range
                                  && tail.Range.EndLine > tail.Range.StartLine
                                  && not (spansDirective source tail.Range)
                                  // size the WRAP by the tail's own extent, not
                                  // tailLineCount: the last bang can sit inside
                                  // a nested CE in an earlier binding, and that
                                  // anchor once inflated a 2-line tail into a
                                  // wrap that then re-wrapped itself every pass
                                  && tail.Range.EndLine - tail.Range.StartLine + 1 - functionDefLines tail.Range >= 4
                                  && not (alreadyWrappedTail tail)
                                  ->
                                  let returns =
                                      index.Exprs
                                      |> Array.filter (fun (_, e) ->
                                          match e with
                                          | SynExpr.YieldOrReturn _ ->
                                              Range.rangeContainsRange tail.Range e.Range && inResumableBody e.Range
                                          | _ -> false)

                                  let terminal = terminalOf tail

                                  let terminalReturn =
                                      match terminal with
                                      | SynExpr.YieldOrReturn _ -> Some terminal.Range
                                      | _ -> None

                                  let returnShapeOk =
                                      match terminalReturn with
                                      | Some tr ->
                                          returns.Length = 1 && Range.equals (returns |> Array.head |> snd).Range tr
                                      | None -> returns.Length = 0

                                  let tailLines = linesOf source tail.Range.StartLine tail.Range.EndLine
                                  let tailIndent = leadingSpaces (List.head tailLines)
                                  let ind = String.replicate tailIndent " "

                                  // strip the terminal `return` so the value
                                  // expression becomes the function's result
                                  let strippedLines =
                                      match terminalReturn with
                                      | Some tr ->
                                          let i = tr.StartLine - tail.Range.StartLine
                                          let line = List.item i tailLines

                                          if line.Substring(tr.StartColumn).StartsWith "return " then
                                              tailLines
                                              |> List.mapi (fun j l ->
                                                  if j = i then
                                                      l.Substring(0, tr.StartColumn) + l.Substring(tr.StartColumn + 7)
                                                  else
                                                      l)
                                              |> Some
                                          else
                                              None
                                      | None -> Some tailLines

                                  let closureEdits =
                                      match strippedLines with
                                      | Some lines when
                                          returnShapeOk
                                          && multiLineStringSafe lines
                                          && not (spansMultiLineLiteral index tail.Range.StartLine tail.Range.EndLine)
                                          && not (mentionsForeignMutable tail.Range (String.concat "\n" lines))
                                          ->
                                          let indented =
                                              lines
                                              |> List.map (fun l -> if isBlank l then "" else "    " + l)
                                              |> String.concat "\n"

                                          let call =
                                              match terminalReturn with
                                              | Some _ -> $"return {fnName} ()"
                                              | None -> $"{fnName} ()"

                                          [ Range.mkRange fileName (Position.mkPos tail.Range.StartLine 0) tail.Range.End,
                                            $"{ind}let {fnName} () =\n{indented}\n{ind}{call}" ]
                                      | _ -> []

                                  if not closureEdits.IsEmpty then
                                      closureEdits
                                  elif
                                      // EARLY RETURNS in the tail: a plain
                                      // closure cannot carry them, but a
                                      // task-returning local function can —
                                      // the tail stays a REAL task body
                                      // (returns and use bindings legal),
                                      // consumed with return!, and the outer
                                      // machine sheds the lines all the same
                                      multiLineStringSafe tailLines
                                      && not (spansMultiLineLiteral index tail.Range.StartLine tail.Range.EndLine)
                                      && not (mentionsForeignMutable tail.Range (String.concat "\n" tailLines))
                                  then
                                      let indented =
                                          tailLines
                                          |> List.map (fun l -> if isBlank l then "" else "    " + l)
                                          |> String.concat "\n"

                                      [ Range.mkRange fileName (Position.mkPos tail.Range.StartLine 0) tail.Range.End,
                                        $"{ind}let {fnName} () = {builder} {{\n{indented}\n{ind}}}\n{ind}return! {fnName} ()" ]
                                  else
                                      []
                              | _ -> []

                          tailFixOffered <- not tailEdits.IsEmpty

                          { Range = noteRange
                            Kind = AdviceKind.ExtractTail tailLineCount
                            Edits = tailEdits }

                  // d) an awaiting suffix the tail wrap cannot carry — early
                  // returns, try/finally AROUND the awaits — can still split
                  // off: as a task-returning local function defined above
                  // the builder, consumed with return!. Returns and use
                  // bindings stay legal because the block remains a real
                  // task body; the machines just get smaller
                  if not tailFixOffered then
                      // the terminal step plus the contiguous run of
                      // bang-free plain steps directly before it; every
                      // step's binding patterns come back with their ranges,
                      // so the ones landing before the block (a plain run a
                      // later bang reset, included) still count as prefix
                      let rec suffixWalk (e: SynExpr) (runStart: range option) (pats: (SynPat * range) list) =
                          match e with
                          | SynExpr.LetOrUse lou ->
                              let pats =
                                  pats @ (lou.Bindings |> List.map (fun (SynBinding(headPat = p)) -> p, e.Range))

                              if
                                  not (lou.IsBang || lou.IsUse)
                                  && lou.Bindings
                                     |> List.forall (fun b -> not (containsBang b.RangeOfBindingWithRhs))
                              then
                                  let start = runStart |> Option.defaultValue e.Range
                                  suffixWalk lou.Body (Some start) pats
                              else
                                  suffixWalk lou.Body None pats
                          | SynExpr.Sequential(expr1 = a; expr2 = b) ->
                              if containsBang a.Range then
                                  suffixWalk b None pats
                              else
                                  let start = runStart |> Option.defaultValue a.Range
                                  suffixWalk b (Some start) pats
                          | terminal -> runStart, pats, terminal

                      let runStart, allPats, terminal = suffixWalk body None []
                      let blockStart = (runStart |> Option.defaultValue terminal.Range).StartLine
                      let blockRange = Range.mkRange fileName (Position.mkPos blockStart 0) body.Range.End
                      let blockLineCount = body.Range.EndLine - blockStart + 1

                      let prefixRange =
                          Range.mkRange fileName body.Range.Start (Position.mkPos blockStart 0)

                      let prefixNames =
                          allPats
                          |> List.filter (fun (_, declRange) -> declRange.StartLine < blockStart)
                          |> List.map (fst >> patIdents)
                          |> List.fold
                              (fun acc cur ->
                                  match acc, cur with
                                  | Some a, Some c -> Some(a @ c)
                                  | _ -> None)
                              (Some [])

                      match freshName source "runRest", prefixNames with
                      | Some fnName, Some boundBefore when
                          containsBang terminal.Range
                          && blockLineCount >= 10
                          // the split only pays when an await REMAINS behind
                          && containsBang prefixRange
                          && startsOwnLine source fe.Range
                          && not (spansDirective source blockRange)
                          ->
                          let blockLines = linesOf source blockStart body.Range.EndLine
                          let blockText = String.concat "\n" blockLines
                          let blockIndent = leadingSpaces (List.head blockLines)

                          // the function lives OUTSIDE the CE: the block may
                          // reference nothing the remaining prefix binds, no
                          // foreign local mutable, and must re-indent safely
                          let referencesPrefix =
                              boundBefore
                              |> List.exists (fun name -> Regex.IsMatch(blockText, identifierPattern name))

                          let shift = (fe.Range.StartColumn + 8) - blockIndent

                          let shifted =
                              if shift > 0 then
                                  Some(
                                      blockLines
                                      |> List.map (fun l -> if isBlank l then "" else String.replicate shift " " + l)
                                  )
                              elif shift = 0 then
                                  Some blockLines
                              else
                                  dedentBy -shift blockLines

                          match shifted with
                          | Some lines when
                              not referencesPrefix
                              && multiLineStringSafe blockLines
                              && not (mentionsForeignMutable blockRange blockText)
                              ->
                              let fnDef =
                                  taskIndentText
                                  + $"let {fnName} () =\n"
                                  + taskIndentText
                                  + $"    {builder} {{\n"
                                  + String.concat "\n" lines
                                  + "\n"
                                  + taskIndentText
                                  + "    }\n"

                              let bodyIndentText = String.replicate blockIndent " "

                              { Range = blockRange
                                Kind = AdviceKind.ExtractAwaitingSuffix blockLineCount
                                Edits =
                                  [ Range.mkRange fileName fe.Range.Start fe.Range.Start,
                                    fnDef.TrimStart() + taskIndentText
                                    blockRange, $"{bodyIndentText}return! {fnName} ()" ] }
                          | _ -> ()
                      | _ -> ()
          | _ -> () ]
