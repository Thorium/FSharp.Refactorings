/// Refactoring (FR0142, fix): a test that BLOCKS on async work returns the
/// work instead.
///
///     [<Fact>]                                [<Fact>]
///     let ``reads`` () =                      let ``reads`` () =
///         let res = load () |> Async.RunSynchronously      task {
///         Assert.Equal(1, res.X)        →          let! res = load () |> Async.StartImmediateAsTask
///                                                  Assert.Equal(1, res.X)
///                                              }
///                                              :> System.Threading.Tasks.Task
///
/// xUnit, NUnit 3+ and MSTest all run a `Task`-returning test and await it,
/// so the thread that ran the test is free while the work is in flight
/// instead of parked in `RunSynchronously`, `.Result`, `.Wait()` or
/// `GetAwaiter().GetResult()`. A test method's shape is the framework's
/// business, not a consumer's — no API changes.
///
/// Safety rules:
///   - the binding carries a test attribute of a framework that awaits
///     `Task` (Fact, Theory, Test, TestCase, TestMethod); FsCheck's
///     `Property` and Expecto's builders are not that
///   - no return type annotation, and the body is not already a `task`/
///     `async` computation
///   - only blocking sites on the body's own statement spine move: a
///     `let x = <blocking>` becomes `let! x = <awaitable>`, a discarded
///     `<blocking> |> ignore` becomes `let! _ = <awaitable>`, a `t.Wait()`
///     becomes `do! t` (a `Task<T>` upcast to `Task` first), and a final
///     blocking statement of a unit-returning test becomes `do!`. Anything
///     nested — inside a lambda, a match arm, a nested CE — leaves the
///     test alone
///   - when the whole body is one blocking expression — the xUnit habit
///     `async { ... } |> Async.RunSynchronously` — there is no block at
///     all: the awaitable is the test, `... |> Async.StartImmediateAsTask
///     :> System.Threading.Tasks.Task`
///   - `Async.RunSynchronously` must be FSharp.Core's, and a `.Result` /
///     `.Wait()` / `GetResult()` receiver must be a Task or ValueTask,
///     both proven by the typed tree
///   - the body starts on its own line under the `=`, so it can be
///     re-indented into the `task { }` block as written
module FSharp.Refactor.TestReturnsTask

open System
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        /// The test's name, for the message.
        Name: string
        /// The whole body — replaced by the `task { }` block.
        Range: range
        OriginalText: string
        ReplacementText: string
        /// How many blocking sites became binds.
        Sites: int
    }

let private testAttributes =
    set [ "Test"; "Fact"; "Theory"; "TestCase"; "TestMethod" ]

/// The frameworks known to await a `Task`-returning test. A same-named
/// attribute from anywhere else — a home-grown `TestAttribute` with a
/// reflection runner, say — would get a Task nobody awaits, and every
/// failure inside it would vanish.
let private awaitingFrameworks =
    [ "Xunit."
      "NUnit.Framework."
      "Microsoft.VisualStudio.TestTools.UnitTesting." ]

let private symbolAt (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    try
        let r = ident.idRange
        let lineText = source.GetLineString(r.EndLine - 1)

        check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ])
        |> Option.map (fun u -> u.Symbol)
    with _ -> // fsharpanalyzer: ignore-line FR0055
        None

let private hasTestAttribute (check: FSharpCheckFileResults) (source: ISourceText) (attributes: SynAttributes) =
    attributes
    |> List.collect (fun l -> l.Attributes)
    |> List.exists (fun a ->
        match a.TypeName with
        | SynLongIdent(id = ids) when not ids.IsEmpty ->
            let last = List.last ids
            let n = last.idText

            (testAttributes.Contains n || testAttributes.Contains(n.Replace("Attribute", "")))
            && (let declared =
                    match symbolAt check source last with
                    | Some(:? FSharpEntity as e) -> e.TryFullName
                    | Some(:? FSharpMemberOrFunctionOrValue as v) ->
                        v.DeclaringEntity |> Option.bind (fun e -> e.TryFullName)
                    | _ -> None

                declared
                |> Option.exists (fun full -> awaitingFrameworks |> List.exists full.StartsWith))
        | _ -> false)

/// The value, function or member an identifier resolves to, if any.
let private valueAt (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    match symbolAt check source ident with
    | Some(:? FSharpMemberOrFunctionOrValue as v) -> Some v
    | _ -> None

/// FSharp.Core's `Async.<name>`, not a same-named member of some other
/// `Async` type in scope.
let private isCoreAsyncMember (name: string) (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    match valueAt check source ident with
    | Some v ->
        (try
            v.DisplayName = name
            && (v.DeclaringEntity
                |> Option.bind (fun e -> e.TryFullName)
                |> Option.exists (fun n -> n = "Microsoft.FSharp.Control.FSharpAsync"))
         with _ -> // fsharpanalyzer: ignore-line FR0055
             false)
    | None -> false

let private taskTypeNames =
    [ "System.Threading.Tasks.Task"; "System.Threading.Tasks.ValueTask" ]

let private isTaskType (t: FSharpType) =
    try
        t.HasTypeDefinition
        && (t.TypeDefinition.TryFullName
            |> Option.exists (fun n -> taskTypeNames |> List.exists (fun tn -> n = tn || n.StartsWith(tn + "`"))))
    with _ -> // fsharpanalyzer: ignore-line FR0055
        false

/// The identifier that decides a receiver's type: `t` in `t`, `x.T` in
/// `x.T`, `M` in `M()` / `x.M(a)`.
[<TailCall>]
let rec private receiverIdent (e: SynExpr) =
    match e with
    | SynExpr.Ident id -> Some id
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
    | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
    | SynExpr.App(funcExpr = f) -> receiverIdent f
    | SynExpr.TypeApp(expr = inner)
    | SynExpr.Paren(expr = inner) -> receiverIdent inner
    | _ -> None

/// Is the value this identifier names typed as a Task or ValueTask — or,
/// for a function or member, does it return one? Yields the number of
/// type arguments: 0 for a plain `Task`, 1 for `Task<T>`.
let rec private isUnitType (t: FSharpType) =
    try
        if t.IsAbbreviation then
            isUnitType t.AbbreviatedType
        else
            t.HasTypeDefinition
            && (t.TypeDefinition.TryFullName
                |> Option.exists (fun n -> n = "Microsoft.FSharp.Core.Unit" || n = "Microsoft.FSharp.Core.unit"))
    with _ -> // fsharpanalyzer: ignore-line FR0055
        false

/// The type of the value an identifier names — for a function or member,
/// what it returns.
let private valueTypeOf (check: FSharpCheckFileResults) (source: ISourceText) (id: Ident) =
    match valueAt check source id with
    | Some v ->
        (try
            Some(
                if v.IsMember || v.IsFunction then
                    v.ReturnParameter.Type
                else
                    v.FullType
            )
         with _ -> // fsharpanalyzer: ignore-line FR0055
             None)
    | None -> None

let private taskArity check source id =
    valueTypeOf check source id
    |> Option.bind (fun t -> if isTaskType t then Some t.GenericArguments.Count else None)

let private isTaskTyped check source id = (taskArity check source id).IsSome

/// A plain `Task` or a `Task<unit>` — something `do!` binds as it is.
let private taskResultIsUnit check source id =
    match valueTypeOf check source id with
    | Some t when isTaskType t -> t.GenericArguments.Count = 0 || isUnitType t.GenericArguments.[0]
    | _ -> false

/// Is this computation provably an `Async<unit>`? Only what the typed tree
/// can name: a value, a call. An `async { }` literal is not, and gets the
/// `Async.Ignore` a `do!` needs regardless.
let private computationResultIsUnit check source (comp: SynExpr) =
    receiverIdent comp
    |> Option.bind (valueTypeOf check source)
    |> Option.exists (fun t ->
        try
            t.HasTypeDefinition
            && t.TypeDefinition.TryFullName = Some "Microsoft.FSharp.Control.FSharpAsync`1"
            && isUnitType t.GenericArguments.[0]
        with _ -> // fsharpanalyzer: ignore-line FR0055
            false)

/// Does this test's body evaluate to unit? The final expression of a
/// unit-typed body may become a `do!`; a value-typed one has no bind shape.
let private returnsUnit (check: FSharpCheckFileResults) (source: ISourceText) (id: Ident) =
    match valueAt check source id with
    | Some v ->
        (try
            isUnitType v.ReturnParameter.Type
         with _ -> // fsharpanalyzer: ignore-line FR0055
             false)
    | None -> false

/// `recv.M` in either parse shape: an expression receiver — `(f ()).M`,
/// a DotGet — or a dotted name — `t.M`, `x.t.M`, one LongIdent. Yields the
/// receiver's text, the identifier that decides its type, and the member.
let private dotMember (source: ISourceText) (e: SynExpr) =
    match e with
    | SynExpr.DotGet(expr = recv; longDotId = SynLongIdent(id = [ m ])) ->
        receiverIdent recv
        |> Option.map (fun rid -> textOfRange source recv.Range, rid, m)
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when ids.Length >= 2 ->
        let m = List.last ids
        let prefix = ids |> List.take (ids.Length - 1)

        let receiverRange =
            Range.unionRanges (List.head prefix).idRange (List.last prefix).idRange

        Some(textOfRange source receiverRange, List.last prefix, m)
    | _ -> None

/// `recv.M()` — a member call with a unit argument, in either shape.
let private (|UnitCall|_|) (e: SynExpr) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = f; argExpr = SynExpr.Const(SynConst.Unit, _)) -> Some f
    | _ -> None

/// A blocking expression and the awaitable to bind instead.
type private Blocking =
    {
        /// The whole blocking expression.
        Site: range
        /// Text of the awaitable: `load () |> Async.StartImmediateAsTask`, or the task itself.
        Awaitable: string
        /// The awaitable as a `do!` operand: the same text, except a
        /// `Task<T>` that was only waited on is upcast to a plain `Task`,
        /// since `do!` needs a unit result.
        DoText: string
        /// True when the result is unit by construction (`.Wait()`).
        UnitResult: bool
        /// False for `.Wait()`: the site yields no value, so `let x =
        /// t.Wait()` has no `let!` form — binding `t` would retype `x`.
        BindsValue: bool
    }

let private (|AsyncModule|_|) (e: SynExpr) =
    match e with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ m; f ])) when m.idText = "Async" -> Some f
    | _ -> None

let private blocking site awaitable =
    { Site = site
      Awaitable = awaitable
      DoText = awaitable
      UnitResult = false
      BindsValue = true }

let private blockingOf (check: FSharpCheckFileResults) (source: ISourceText) (e: SynExpr) : Blocking option =
    match stripParens e with
    // `comp |> Async.RunSynchronously`
    | PipeApp(comp, (AsyncModule f as rhs)) when
        f.idText = "RunSynchronously"
        && isCoreAsyncMember "RunSynchronously" check source f
        ->
        match stripParens comp with
        // `task { ... } |> Async.AwaitTask |> Async.RunSynchronously`: the
        // task was awaitable all along — both pipes go
        | PipeApp(inner, AsyncModule aw) when aw.idText = "AwaitTask" && isCoreAsyncMember "AwaitTask" check source aw ->
            Some
                { blocking e.Range (textOfRange source inner.Range) with
                    UnitResult = receiverIdent inner |> Option.exists (taskResultIsUnit check source) }
        | _ ->
            // everything up to the operand — the pipe and its line break,
            // if the pipe opened a new line — stays as written
            let upToOperand =
                textOfRange source (Range.mkRange comp.Range.FileName comp.Range.Start rhs.Range.Start)

            Some
                { blocking e.Range $"{upToOperand}Async.StartImmediateAsTask" with
                    UnitResult = computationResultIsUnit check source comp }
    // `Async.RunSynchronously comp` / `Async.RunSynchronously(comp)` — a
    // timeout or cancellation argument is a different contract, left alone
    | SynExpr.App(isInfix = false; funcExpr = AsyncModule f; argExpr = arg) when
        f.idText = "RunSynchronously"
        && isCoreAsyncMember "RunSynchronously" check source f
        ->
        match stripParens arg with
        | SynExpr.Tuple _ -> None
        | comp ->
            Some
                { blocking e.Range $"{atomicText source comp} |> Async.StartImmediateAsTask" with
                    UnitResult = computationResultIsUnit check source comp }
    // `t.Wait()` — unit by construction — and `t.GetAwaiter().GetResult()`
    | UnitCall f ->
        match dotMember source f with
        | Some(recvText, rid, m) when m.idText = "Wait" ->
            match taskArity check source rid with
            | Some arity ->
                Some
                    { blocking e.Range recvText with
                        DoText =
                            if arity = 0 then
                                recvText
                            else
                                $"({recvText} :> System.Threading.Tasks.Task)"
                        UnitResult = true
                        BindsValue = false }
            | None -> None
        | Some(_, _, gr) when gr.idText = "GetResult" ->
            match f with
            | SynExpr.DotGet(expr = UnitCall inner) ->
                match dotMember source inner with
                | Some(recvText, rid, ga) when ga.idText = "GetAwaiter" ->
                    match taskArity check source rid with
                    // a plain Task's GetResult() is unit: a `do!` site
                    | Some _ ->
                        Some
                            { blocking e.Range recvText with
                                UnitResult = taskResultIsUnit check source rid }
                    | None -> None
                | _ -> None
            | _ -> None
        | _ -> None
    // `t.Result`
    | other ->
        match dotMember source other with
        | Some(recvText, rid, p) when p.idText = "Result" && isTaskTyped check source rid ->
            Some
                { blocking e.Range recvText with
                    UnitResult = taskResultIsUnit check source rid }
        | _ -> None

/// `<blocking> |> ignore` — the result was thrown away.
let private (|Ignored|_|) (e: SynExpr) =
    match stripParens e with
    | PipeApp(inner, SynExpr.Ident id) when id.idText = "ignore" -> Some inner
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.Ident id; argExpr = inner) when id.idText = "ignore" -> Some inner
    | _ -> None

/// One edit inside the body: (range, replacement).
type private Edit = range * string

/// Walk the body's statement spine collecting the edits that turn each
/// blocking statement into a bind. None when a blocking site sits where a
/// bind cannot go (a final expression whose result is not unit).
let rec private spineEdits
    (check: FSharpCheckFileResults)
    (source: ISourceText)
    (bodyIsUnit: bool)
    (e: SynExpr)
    (isLast: bool)
    : Edit list option =
    let spineEdits = spineEdits check source bodyIsUnit
    let statementEdit = statementEdit check source bodyIsUnit

    match e with
    | LetOrUseE lou when not lou.IsBang ->
        let own =
            match lou.Bindings with
            // `let mutable x = <blocking>` has no `let! mutable` form, and
            // a local FUNCTION's body is a closure: `let f () = <blocking>`
            // runs when f is called, not here
            | [ SynBinding(isMutable = false; headPat = pat; expr = rhs; trivia = trivia) ] when
                not lou.IsUse
                && (match pat with
                    | SynPat.LongIdent(argPats = SynArgPats.Pats(_ :: _)) -> false
                    | _ -> true)
                ->
                match blockingOf check source rhs with
                // `let x = t.Wait()` binds unit; `let! x = t` would retype
                // x — that site stays as it is
                | Some b when not b.BindsValue -> Some []
                | Some b ->
                    // `let x = <blocking>` → `let! x = <awaitable>`; the
                    // `!` moves the expression one column right, and a
                    // continuation line aligned with it must follow
                    let kw = trivia.LeadingKeyword.Range
                    Some [ (kw, "let!"); (b.Site, b.Awaitable.Replace("\n", "\n ")) ]
                | None -> Some []
            | _ -> Some []

        match own, spineEdits lou.Body isLast with
        | Some a, Some b -> Some(a @ b)
        | _ -> None
    | SynExpr.Sequential(expr1 = a; expr2 = b) ->
        match statementEdit a false, spineEdits b isLast with
        | Some x, Some y -> Some(x @ y)
        | _ -> None
    // `match <blocking> with` → `match! <awaitable> with`; the arms are
    // not on the spine, so a blocking site inside one stays
    | SynExpr.Match(expr = scrutinee; trivia = trivia) ->
        match blockingOf check source scrutinee with
        | Some b -> Some [ (trivia.MatchKeyword, "match!"); (b.Site, b.Awaitable.Replace("\n", "\n ")) ]
        | None -> Some []
    | last -> statementEdit last true

/// A statement position: a discarded blocking call becomes `let! _ =`, a
/// unit-typed one `do!`. In FINAL position the blocking result IS the
/// test's result: `do!` when the test returns unit, otherwise there is no
/// bind shape and the rewrite stops.
and private statementEdit
    (check: FSharpCheckFileResults)
    (source: ISourceText)
    (bodyIsUnit: bool)
    (e: SynExpr)
    (isLast: bool)
    =
    // a prefix in front of the expression moves its first line right; its
    // continuation lines — a pipe opening a new line at the statement's own
    // column — must follow, or the operator lands offside (Fuuga)
    let prefixed (prefix: string) (text: string) =
        prefix + text.Replace("\n", "\n" + String(' ', prefix.Length))

    match e with
    | Ignored inner ->
        match blockingOf check source inner with
        // a discarded result the typed tree proves unit is a `do!`; any
        // other stays `let! _ =` — no `Async.Ignore` to read past
        | Some b when b.UnitResult -> Some [ (e.Range, prefixed "do! " b.DoText) ]
        // a block cannot end on a bind: a final discarded site gets the
        // `()` the `ignore` used to supply, at the statement's own column
        | Some b when isLast ->
            Some [ (e.Range, prefixed "let! _ = " b.Awaitable + $"\n{String(' ', e.Range.StartColumn)}()") ]
        | Some b -> Some [ (e.Range, prefixed "let! _ = " b.Awaitable) ]
        | None -> Some []
    | _ ->
        match blockingOf check source e with
        | Some b when b.UnitResult || (isLast && bodyIsUnit) -> Some [ (e.Range, prefixed "do! " b.DoText) ]
        | Some _ when isLast -> None
        | Some b -> Some [ (e.Range, prefixed "let! _ = " b.Awaitable) ]
        | None -> Some []

/// A body holding a lock or a thread-bound handle across the work: after
/// a bind the rest may run on another thread, and `Monitor.Exit` or
/// `ReleaseMutex` from the wrong thread throws.
let private threadBound (source: ISourceText) (body: SynExpr) =
    let text = textOfRange source body.Range

    [ "Monitor."
      "Mutex"
      "ReaderWriterLock"
      "ThreadStatic"
      "ThreadLocal"
      "WaitOne" ]
    |> List.exists text.Contains

/// Does the body already run as a computation, or return a Task?
let private alreadyComputation (body: SynExpr) =
    match stripParens body with
    | SynExpr.App(funcExpr = SynExpr.Ident b; argExpr = SynExpr.ComputationExpr _) ->
        b.idText = "task" || b.idText = "async" || b.idText = "backgroundTask"
    | SynExpr.Upcast _ -> true
    | _ -> false

/// Apply edits to the body text (bottom-up), re-indent every line by four,
/// and wrap in `task { ... } :> System.Threading.Task` at the body's own
/// indentation.
let private wrapBody (source: ISourceText) (body: SynExpr) (edits: Edit list) =
    let bodyText = textOfRange source body.Range
    let start = body.Range.Start

    // an edit's position relative to the body text: line starts are read
    // off the body text itself (a `\r` before the break stays inside its
    // line), and the first line begins at the body's own column
    let lineStarts =
        let starts = ResizeArray<int>([ 0 ])

        for i in 0 .. bodyText.Length - 1 do
            if bodyText.[i] = '\n' then
                starts.Add(i + 1)

        starts

    let offsetOf (p: pos) =
        let relativeLine = p.Line - start.Line

        let column =
            if relativeLine = 0 then
                p.Column - start.Column
            else
                p.Column

        lineStarts.[relativeLine] + column

    let edited =
        edits
        |> List.sortByDescending (fun (r, _) -> r.StartLine, r.StartColumn)
        |> List.fold
            (fun (text: string) (r, replacement) ->
                let s = offsetOf r.Start
                let e = offsetOf r.End
                text.Substring(0, s) + replacement + text.Substring e)
            bodyText

    let indent = String(' ', start.Column)
    let inner = indent + "    "

    let lines =
        edited.Replace("\r\n", "\n").Split '\n'
        |> Array.mapi (fun i line ->
            // lines after the first carry their original leading columns;
            // the first sits at the body column, which the text lost
            if line.Trim() = "" then ""
            elif i = 0 then inner + line
            else "    " + line)

    // the upcast shares the closing brace's line: on a line of its own it
    // is offside of the `task` expression and does not parse
    String.concat
        "\n"
        ([ indent + "task {" ]
         @ List.ofArray lines
         @ [ indent + "} :> System.Threading.Tasks.Task" ])

/// Every test binding in the file: module-level lets and type members.
let private testBindings (index: AstIndex.Index) =
    [ for _, decl in index.Decls do
          match decl with
          | SynModuleDecl.Let(bindings = bindings) ->
              for b in bindings do
                  yield b
          | SynModuleDecl.Types(typeDefns = defns) ->
              for SynTypeDefn(members = members; typeRepr = repr) in defns do
                  for m in members do
                      match m with
                      | SynMemberDefn.Member(memberDefn = b) -> yield b
                      | _ -> ()

                  match repr with
                  | SynTypeDefnRepr.ObjectModel(members = objMembers) ->
                      for m in objMembers do
                          match m with
                          | SynMemberDefn.Member(memberDefn = b) -> yield b
                          | _ -> ()
                  | _ -> ()
          | _ -> () ]

let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for binding in testBindings index do
              match binding with
              | SynBinding(attributes = attributes; headPat = headPat; returnInfo = None; expr = body) when
                  hasTestAttribute check source attributes
                  && not (alreadyComputation body)
                  && not (threadBound source body)
                  ->
                  let nameIdent =
                      match headPat with
                      | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
                      | _ -> None

                  // the body must open on its own line, under the header,
                  // so its lines re-indent as written
                  let ownLine =
                      body.Range.StartLine > headPat.Range.EndLine
                      && (source.GetLineString(body.Range.StartLine - 1)).Substring(0, body.Range.StartColumn).Trim() = ""

                  match nameIdent with
                  | Some id ->
                      let bodyIsUnit = returnsUnit check source id

                      let suggestion replacement sites =
                          { Name = id.idText
                            Range = body.Range
                            OriginalText = textOfRange source body.Range
                            ReplacementText = replacement
                            Sites = sites }

                      match blockingOf check source body with
                      // the whole body blocks on one thing — `async { ... }
                      // |> Async.RunSynchronously` — so the awaitable IS the
                      // test: no block, just the upcast
                      | Some b when b.UnitResult || bodyIsUnit ->
                          yield suggestion $"{b.Awaitable} :> System.Threading.Tasks.Task" 1
                      | Some _ -> ()
                      | None ->
                          // re-indenting the body would also re-indent the
                          // inside of a string literal that spans lines —
                          // a triple-quoted expected value, say — and that
                          // compiles with different content
                          let spansLinesInBody (r: range) =
                              r.StartLine <> r.EndLine && Range.rangeContainsRange body.Range r

                          let multiLineString =
                              (index.Exprs
                               |> Seq.exists (fun (_, e) ->
                                   match e with
                                   | SynExpr.Const(SynConst.String _, r)
                                   | SynExpr.Const(SynConst.Bytes _, r)
                                   | SynExpr.InterpolatedString(range = r) -> spansLinesInBody r
                                   | _ -> false))
                              || (index.Pats
                                  |> Seq.exists (fun (_, p) ->
                                      match p with
                                      | SynPat.Const(SynConst.String _, r)
                                      | SynPat.Const(SynConst.Bytes _, r) -> spansLinesInBody r
                                      | _ -> false))

                          if ownLine && not multiLineString then
                              match spineEdits check source bodyIsUnit body true with
                              | Some edits when not edits.IsEmpty ->
                                  let sites =
                                      edits |> List.filter (fun (_, t) -> t <> "let!" && t <> "match!") |> List.length

                                  yield
                                      suggestion ((wrapBody source body edits).Substring(body.Range.StartColumn)) sites
                              | _ -> ()
                  | None -> ()
              | _ -> () ]
