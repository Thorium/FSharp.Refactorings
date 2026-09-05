/// Refactoring (correctness): a locally constructed disposable bound with
/// `let` has no owner — nothing ever disposes it.
///
///     let stream = new FileStream(path, FileMode.Open)     // leaked
///     use stream = new FileStream(path, FileMode.Open)     // disposed at
///                                                          // scope exit
///
/// The sibling of FR0032 (disposable FIELDS without IDisposable), for
/// expression-level bindings.
///
/// Three tiers, decided by where the bare mentions of the binder send it:
///   - FIX (`let` → `use`) when the value provably stays inside the scope:
///     every mention is a member access (`x.Read ...`) or a comparison
///     operand, never inside a lambda (which may outlive the scope), and
///     no result position mentions it at all
///   - NOTHING when every escape is an ownership transfer: the value is
///     the scope's result (the caller owns it — the factory pattern), or an
///     argument to the constructor of another disposable, which disposes
///     it in turn (HttpClient its handler, StreamReader its stream). A
///     `use` here would dispose it under the new owner
///   - NOTE ONLY when a mention could move the value somewhere whose
///     ownership is unknown — passed to an ordinary function, stored,
///     captured by a lambda: the leak is worth pointing out, the rewrite
///     is the author's call, and the note names the destination
///
/// Skips entirely when the scope already calls `x.Dispose()` — that is
/// manual management, not a leak.
module FSharp.Refactor.UseBinding

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

/// A scope whose leaks weigh differently from an ordinary function's.
[<RequireQualifiedAccess>]
type ScopeContext =
    /// `[<EntryPoint>]`: nothing accumulates, but nothing flushes either —
    /// .NET runs no finalizers at process exit, so a writer's last buffer
    /// or a transaction's last work is lost. Only reported for such types.
    | EntryPoint
    /// an ASP.NET action or SignalR hub method: the scope runs once per
    /// request, so the leak repeats until the pool behind it runs dry
    | RequestHandler

type Suggestion =
    {
        /// The `let` keyword's range (the fix rewrites it to `use`).
        Range: range
        Name: string
        /// None = advisory only (the value may escape the scope).
        Fix: (string * string) option
        /// Where an advisory's value goes, when a name for it is known:
        /// the function it is passed to, the type it is stored in.
        Destination: string option
        /// True when the destination is a function in this file whose body
        /// was read and does not dispose the parameter.
        DestinationInspected: bool
        /// The kind of scope the binding sits in, when it is one whose
        /// leaks weigh differently: the program's entry point, a request
        /// handler.
        Context: ScopeContext option
    }

/// BCL factories whose result the caller owns, by enclosing type.
let private bclFactories =
    [ "System.IO.File",
      set
          [ "Open"
            "OpenRead"
            "OpenWrite"
            "OpenText"
            "Create"
            "CreateText"
            "AppendText" ]
      "System.Xml.XmlReader", set [ "Create" ]
      "System.Xml.XmlWriter", set [ "Create" ] ]

/// The identifier an application's function position names, if it is a
/// plain (possibly dotted, possibly type-applied) name.
let private headIdent (f: SynExpr) =
    match f with
    | SynExpr.Ident id -> ValueSome id
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> ValueSome(List.last ids)
    | SynExpr.TypeApp(expr = SynExpr.Ident id) -> ValueSome id
    | SynExpr.TypeApp(expr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))) when not ids.IsEmpty ->
        ValueSome(List.last ids)
    | _ -> ValueNone

let private symbolAt (check: FSharpCheckFileResults) (source: ISourceText) (id: Ident) =
    let r = id.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ id.idText ])
    |> Option.map (fun u -> u.Symbol)

/// Is the bound expression a construction the binder OWNS — `new T(...)`,
/// a constructor applied without `new` (`MemoryStream()`), or an
/// ownership-transferring BCL factory (`File.OpenRead path` is THE way to
/// open a file, and it leaks exactly like a bare constructor)?
let private locallyConstructed (check: FSharpCheckFileResults) (source: ISourceText) (rhs: SynExpr) =
    match rhs with
    | SynExpr.New _ -> true
    | SynExpr.App(isInfix = false; funcExpr = f) ->
        let headIdent = headIdent f

        // cheap prefilter before paying for symbol resolution: a
        // constructor-without-new is spelled with a type name and the BCL
        // factories are PascalCase, while ordinary calls (`let x = load y`)
        // are lowercase — resolving those for every let in a sweep put
        // this rule near the top of the slow-analyzer list
        let plausible =
            match headIdent with
            | ValueSome id -> id.idText.Length > 0 && System.Char.IsUpper id.idText.[0]
            | ValueNone -> false

        match (if plausible then headIdent else ValueNone) with
        | ValueSome id ->
            match symbolAt check source id with
            | Some(:? FSharpMemberOrFunctionOrValue as value) ->
                (try
                    value.IsConstructor
                 with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                     false)
                || (let enclosing = OptionModule.enclosingFullName value

                    bclFactories
                    |> List.exists (fun (entity, names) -> enclosing = entity && names.Contains id.idText))
            | _ -> false
        | ValueNone -> false
    | _ -> false

/// Does the identifier name a constructor of a disposable type — or, as
/// the type name of a `new T(...)`, a disposable type T?
let private constructsDisposable (check: FSharpCheckFileResults) (source: ISourceText) (id: Ident) =
    match symbolAt check source id with
    | Some(:? FSharpMemberOrFunctionOrValue as value) ->
        (try
            value.IsConstructor
            && value.DeclaringEntity |> Option.exists ObjectDesign.entityIsDisposable
         with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
             false)
    | Some(:? FSharpEntity as entity) -> ObjectDesign.entityIsDisposable entity
    | _ -> false

let private typeIdent (t: SynType) =
    match t with
    | SynType.LongIdent(SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
    | SynType.App(typeName = SynType.LongIdent(SynLongIdent(id = ids))) when not ids.IsEmpty -> Some(List.last ids)
    | _ -> None

/// Where a bare mention of the binder sends the value.
type private Escape =
    /// the scope's result: the caller owns it
    | Returned
    /// an argument to the constructor of another disposable, which
    /// disposes it in turn
    | Adopted
    /// an operand of a comparison: it goes nowhere
    | Kept
    /// passed to a function, stored, captured — owner unknown; the name
    /// of the destination when one can be read off the syntax
    | Handed of string option
    /// passed to a same-file function by bare name: the argument position
    /// (curried index, tuple element) lets the callee's body be read
    | HandedAt of callee: string * argument: int * element: int option

let private nameOf (e: SynExpr) =
    match headIdent e with
    | ValueSome id -> Some id.idText
    | ValueNone -> None

/// The compiled name of an infix operator's function position
/// (`op_PipeRight` for `|>`), empty for anything else.
let private operatorName (op: SynExpr) =
    match headIdent op with
    | ValueSome id -> id.idText
    | ValueNone -> ""

/// The head of a curried application chain `f a b` = App(App(f, a), b):
/// the identifier, whether it was spelled bare (a single segment, so a
/// same-file binding of that name is the callee), and how many arguments
/// the chain has already applied.
let rec private chainHead (f: SynExpr) (applied: int) =
    match f with
    | SynExpr.App(isInfix = false; funcExpr = g) -> chainHead g (applied + 1)
    | SynExpr.Ident id -> ValueSome id, true, applied
    | _ -> headIdent f, false, applied

let private handed (f: SynExpr) (element: int option) check source =
    match chainHead f 0 with
    | ValueSome id, _, _ when constructsDisposable check source id -> Adopted
    | ValueSome id, true, applied -> HandedAt(id.idText, applied, element)
    | ValueSome id, false, _ -> Handed(Some id.idText)
    | ValueNone, _, _ -> Handed None

/// Walk outward from a mention through the nodes that merely wrap an
/// argument (parens, tuples, annotations, upcasts, named-argument `=`)
/// to the construction or call that receives it. `element` remembers
/// which tuple element the mention sat in, for reading the callee.
[<TailCall>]
let rec private classifyLoop
    check
    source
    (mention: range)
    (viaEquality: bool)
    (element: int option)
    (path: SyntaxNode list)
    =
    match path with
    | SyntaxNode.SynExpr(SynExpr.Tuple(exprs = elements)) :: rest ->
        let at =
            elements
            |> List.tryFindIndex (fun e -> Range.rangeContainsRange e.Range mention)

        classifyLoop check source mention viaEquality (if element.IsSome then element else at) rest
    | SyntaxNode.SynExpr(SynExpr.Paren _ | SynExpr.Typed _ | SynExpr.Upcast _ | SynExpr.InferredUpcast _) :: rest ->
        classifyLoop check source mention viaEquality element rest
    // `x |> f`: the infix node, then the outer application whose argument
    // is the callee (operators parse as LongIdents carrying their notation)
    | SyntaxNode.SynExpr(SynExpr.App(isInfix = true; funcExpr = op)) :: SyntaxNode.SynExpr(SynExpr.App(argExpr = callee)) :: _ when
        operatorName op = "op_PipeRight"
        ->
        handed callee element check source
    // the operator's own node of an infix application: keep climbing
    | SyntaxNode.SynExpr(SynExpr.App(isInfix = true)) :: rest ->
        classifyLoop check source mention viaEquality element rest
    // `Prop = x` is a named argument inside a construction and a plain
    // comparison anywhere else — the next node tells which
    | SyntaxNode.SynExpr(SynExpr.App(isInfix = false; funcExpr = SynExpr.App(isInfix = true; funcExpr = op))) :: rest when
        (let name = operatorName op in name = "op_Equality" || name = "op_Inequality")
        ->
        classifyLoop check source mention true element rest
    | SyntaxNode.SynExpr(SynExpr.New(targetType = t)) :: _ ->
        match typeIdent t with
        | Some id when constructsDisposable check source id -> Adopted
        | Some id -> Handed(Some id.idText)
        | None -> Handed None
    // `owner.Prop <- x`: a disposable owner disposes what it holds
    | SyntaxNode.SynExpr(SynExpr.LongIdentSet(longDotId = SynLongIdent(id = owner :: _ :: _))) :: _
    | SyntaxNode.SynExpr(SynExpr.Set(targetExpr = SynExpr.DotGet(expr = SynExpr.Ident owner))) :: _ ->
        if ObjectDesign.resolvesToDisposable check source owner then
            Adopted
        else
            Handed(Some owner.idText)
    // `owner.Add(x)`: a disposable collection owns its parts
    | SyntaxNode.SynExpr(SynExpr.App(
        isInfix = false
        funcExpr = SynExpr.DotGet(expr = SynExpr.Ident owner; longDotId = SynLongIdent(id = path))
        argExpr = a)) :: _
    | SyntaxNode.SynExpr(SynExpr.App(
        isInfix = false
        funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = owner :: (_ :: _ as path)))
        argExpr = a)) :: _ when
        (List.last path).idText = "Add"
        && Range.rangeContainsRange a.Range mention
        && ObjectDesign.resolvesToDisposable check source owner
        ->
        Adopted
    | SyntaxNode.SynExpr(SynExpr.App(isInfix = false; funcExpr = f; argExpr = a)) :: _ when
        Range.rangeContainsRange a.Range mention
        ->
        handed f element check source
    | SyntaxNode.SynExpr(SynExpr.Lambda _) :: _ -> Handed(Some "a lambda")
    | _ when viaEquality -> Kept
    | _ -> Handed None

/// The names the callee's parameter at (curried index, tuple element)
/// binds — `s` for `let save (s: Stream) =`, for `(name, s)` element 1.
let private parameterNames (pats: SynPat list) (argument: int) (element: int option) =
    let rec strip (p: SynPat) =
        match p with
        | SynPat.Paren(inner, _)
        | SynPat.Typed(pat = inner) -> strip inner
        | _ -> p

    match List.tryItem argument pats |> Option.map strip, element with
    | Some(SynPat.Tuple(elementPats = elements)), Some k ->
        List.tryItem k elements |> Option.map patNames |> Option.defaultValue []
    // a tuple handed to a non-tuple parameter: which element is which is
    // not readable here
    | Some _, Some _ -> []
    | Some p, None -> patNames p
    | None, _ -> []

/// ASP.NET action attributes, and the base types whose members run per
/// request or per message.
let private handlerAttributes =
    set
        [ "HttpGet"
          "HttpPost"
          "HttpPut"
          "HttpDelete"
          "HttpPatch"
          "HttpHead"
          "HttpOptions"
          "Route" ]

let private handlerBases =
    set [ "Controller"; "ControllerBase"; "ApiController"; "Hub" ]

let private lastIdentOf (t: SynType) =
    match t with
    | SynType.LongIdent(SynLongIdent(id = ids))
    | SynType.App(typeName = SynType.LongIdent(SynLongIdent(id = ids))) when not ids.IsEmpty ->
        Some (List.last ids).idText
    | _ -> None

/// The scope kind a binding at `at` sits in: read off the binding's own
/// attributes (`[<EntryPoint>]`, `[<HttpGet>]`) or the base type of the
/// type declaring it (a controller, a hub).
let private bindingContext (path: SyntaxNode list) (at: range) =
    let fromAttributes =
        path
        |> List.tryPick (fun node ->
            match node with
            | SyntaxNode.SynBinding(SynBinding(attributes = attributeLists)) ->
                attributeLists
                |> List.collect (fun l -> l.Attributes)
                |> List.tryPick (fun (a: SynAttribute) ->
                    match a.TypeName with
                    | SynLongIdent(id = ids) when not ids.IsEmpty ->
                        match (List.last ids).idText.Replace("Attribute", "") with
                        | "EntryPoint" -> Some ScopeContext.EntryPoint
                        | name when handlerAttributes.Contains name -> Some ScopeContext.RequestHandler
                        | _ -> None
                    | _ -> None)
            | _ -> None)

    let fromEnclosingType () =
        path
        |> List.tryPick (fun node ->
            match node with
            | SyntaxNode.SynModule(SynModuleDecl.Types(typeDefns = defns)) ->
                defns
                |> List.tryFind (fun (SynTypeDefn(range = r)) -> Range.rangeContainsRange r at)
                |> Option.bind (fun (SynTypeDefn(typeRepr = repr; members = extra)) ->
                    let members =
                        match repr with
                        | SynTypeDefnRepr.ObjectModel(members = ms) -> ms @ extra
                        | _ -> extra

                    members
                    |> List.tryPick (fun m ->
                        match m with
                        | SynMemberDefn.Inherit(baseType = Some t)
                        | SynMemberDefn.ImplicitInherit(inheritType = t) ->
                            lastIdentOf t
                            |> Option.filter handlerBases.Contains
                            |> Option.map (fun _ -> ScopeContext.RequestHandler)
                        | _ -> None))
            | _ -> None)

    match fromAttributes with
    | Some c -> Some c
    | None -> fromEnclosingType ()

/// Types whose undisposed instance loses work at process exit rather than
/// merely holding a handle the OS reclaims: buffered writers and streams,
/// and transactions.
let private flushSensitiveBases =
    set
        [ "System.IO.Stream"
          "System.IO.TextWriter"
          "System.IO.BinaryWriter"
          "System.Data.Common.DbTransaction" ]

let private flushSensitive (check: FSharpCheckFileResults) (source: ISourceText) (binder: Ident) =
    match symbolAt check source binder with
    | Some(:? FSharpMemberOrFunctionOrValue as value) ->
        (try
            let t = OptionModule.stripAbbreviations value.FullType

            let rec baseChain (t: FSharpType) =
                if t.HasTypeDefinition then
                    let entity = t.TypeDefinition

                    (entity.TryFullName |> Option.exists flushSensitiveBases.Contains)
                    || (entity.BaseType |> Option.exists baseChain)
                else
                    false

            baseChain t
            || (t.HasTypeDefinition
                && t.TypeDefinition.AllInterfaces
                   |> Seq.exists (fun i ->
                       i.HasTypeDefinition
                       && i.TypeDefinition.TryFullName = Some "System.Data.IDbTransaction"))
         with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
             false)
    | _ -> false

/// The result-position expressions of a body: what the scope evaluates to.
[<TailCall>]
let rec private resultsLoop (acc: SynExpr list) (pending: SynExpr list) =
    match pending with
    | [] -> acc
    | e :: rest ->
        match e with
        | SynExpr.Sequential(expr2 = e2) -> resultsLoop acc (e2 :: rest)
        | LetOrUseE lou -> resultsLoop acc (lou.Body :: rest)
        | SynExpr.IfThenElse(thenExpr = t; elseExpr = els) ->
            let next =
                els
                |> Option.map (fun e2 -> t :: e2 :: rest)
                |> Option.defaultWith (fun () -> t :: rest)

            resultsLoop acc next
        | SynExpr.Match(clauses = clauses)
        | SynExpr.MatchBang(clauses = clauses) ->
            resultsLoop acc ((clauses |> List.map (fun (SynMatchClause(resultExpr = r)) -> r)) @ rest)
        | SynExpr.TryWith(tryExpr = t; withCases = clauses) ->
            resultsLoop acc (t :: (clauses |> List.map (fun (SynMatchClause(resultExpr = r)) -> r)) @ rest)
        | SynExpr.TryFinally(tryExpr = t) -> resultsLoop acc (t :: rest)
        | SynExpr.Paren(expr = inner)
        | SynExpr.Typed(expr = inner)
        | SynExpr.Upcast(expr = inner)
        | SynExpr.InferredUpcast(expr = inner)
        | SynExpr.YieldOrReturn(expr = inner)
        | SynExpr.YieldOrReturnFrom(expr = inner) -> resultsLoop acc (inner :: rest)
        | SynExpr.While _
        | SynExpr.For _
        | SynExpr.ForEach _ -> resultsLoop acc rest
        | other -> resultsLoop (other :: acc) rest

/// Find leaked local disposables. Requires typed check results for the
/// IDisposable gate.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        // module-level functions of this file, by name: a disposable handed
        // to one of them can be followed one hop into its body
        let sameFileFunctions =
            index.Decls
            |> Array.collect (fun (_, d) ->
                match d with
                | SynModuleDecl.Let(bindings = bindings) ->
                    bindings
                    |> List.choose (fun (SynBinding(headPat = pat; expr = body)) ->
                        match pat with
                        | SynPat.LongIdent(longDotId = SynLongIdent(id = [ fname ]); argPats = SynArgPats.Pats pats) when
                            not pats.IsEmpty
                            ->
                            Some(fname.idText, (pats, body))
                        | _ -> None)
                    |> Array.ofList
                | _ -> [||])
            |> Array.groupBy fst
            |> Array.choose (fun (name, defs) ->
                match defs with
                | [| _, def |] -> Some(name, def)
                | _ -> None)
            |> Map.ofArray

        // does the callee dispose the parameter the value arrives in:
        // `use`-bind it, call `.Dispose()` on it, or hand it to another
        // disposable's constructor?
        let calleeDisposes (pats: SynPat list, body: SynExpr) (argument: int) (element: int option) =
            let names = parameterNames pats argument element |> set

            let mentionsParameter (r: range) =
                index.Exprs
                |> Array.exists (fun (_, e) ->
                    match e with
                    | SynExpr.Ident id -> names.Contains id.idText && Range.rangeContainsRange r id.idRange
                    | _ -> false)

            not names.IsEmpty
            && index.Exprs
               |> Array.exists (fun (path, e) ->
                   Range.rangeContainsRange body.Range e.Range
                   && (match e with
                       | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ p; m ])) ->
                           names.Contains p.idText && m.idText = "Dispose"
                       | SynExpr.DotGet(expr = inner; longDotId = SynLongIdent(id = [ m ])) ->
                           m.idText = "Dispose" && mentionsParameter inner.Range
                       | SynExpr.Ident id when names.Contains id.idText ->
                           let usedDirectly =
                               path
                               |> List.truncate 2
                               |> List.exists (fun n ->
                                   match n with
                                   | SyntaxNode.SynExpr(LetOrUseE lou) when lou.IsUse ->
                                       lou.Bindings
                                       |> List.exists (fun (SynBinding(expr = rhs)) -> rhs.Range = id.idRange)
                                   | _ -> false)

                           usedDirectly || classifyLoop check source id.idRange false None path = Adopted
                       | _ -> false))

        [ for declPath, expr in index.Exprs do
              match expr with
              | LetOrUseE lou when not (lou.IsBang || lou.IsUse || lou.IsRecursive) ->
                  match lou.Bindings with
                  | [ SynBinding(
                          isMutable = false
                          headPat = (SynPat.Named(ident = SynIdent(ident = binder); accessibility = None) | SynPat.LongIdent(
                              longDotId = SynLongIdent(id = [ binder ])
                              argPats = SynArgPats.Pats []
                              accessibility = None))
                          expr = rhs) ] when
                      locallyConstructed check source rhs
                      && ObjectDesign.resolvesToDisposable check source binder
                      ->
                      let name = binder.idText
                      let body = lou.Body

                      // classify every mention of the binder in the scope
                      let mentions =
                          index.Exprs
                          |> Array.filter (fun (_, e) ->
                              match e with
                              | SynExpr.Ident id when id.idText = name ->
                                  Range.rangeContainsRange body.Range id.idRange
                              | SynExpr.LongIdent(longDotId = SynLongIdent(id = firstId :: _)) when
                                  firstId.idText = name
                                  ->
                                  Range.rangeContainsRange body.Range firstId.idRange
                              | _ -> false)

                      let manuallyDisposed =
                          mentions
                          |> Array.exists (fun (_, e) ->
                              match e with
                              | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ _; m ])) -> m.idText = "Dispose"
                              | _ -> false)

                      if not manuallyDisposed then
                          let results = resultsLoop [] [ body ]

                          let inLambda (path: SyntaxNode list) =
                              path
                              |> List.exists (fun node ->
                                  match node with
                                  | SyntaxNode.SynExpr(SynExpr.Lambda _) ->
                                      // only lambdas INSIDE this scope count
                                      true
                                  | _ -> false)

                          // where each bare mention sends the value
                          let escapes =
                              [ for path, e in mentions do
                                    match e with
                                    | SynExpr.Ident _ ->
                                        if inLambda path then
                                            Handed(Some "a lambda")
                                        elif results |> List.exists (fun r -> r.Range = e.Range) then
                                            Returned
                                        else
                                            classifyLoop check source e.Range false None path
                                    | _ -> () ]
                              // a same-file callee is read one hop: disposing
                              // the parameter is an ownership transfer, not
                              // disposing it is a leak this note can name
                              |> List.map (fun escape ->
                                  match escape with
                                  | HandedAt(callee, argument, element) ->
                                      match Map.tryFind callee sameFileFunctions with
                                      | Some target when calleeDisposes target argument element -> Adopted
                                      | Some _ -> escape
                                      | None -> Handed(Some callee)
                                  | other -> other)

                          // a member access in result position (`stream.Length`
                          // as the value) still reads the object after the
                          // scope would have disposed it
                          let inResult =
                              mentions
                              |> Array.exists (fun (path, e) ->
                                  inLambda path
                                  || results |> List.exists (fun r -> Range.rangeContainsRange r.Range e.Range))

                          let handedTo =
                              escapes
                              |> List.tryPick (fun e ->
                                  match e with
                                  | Handed d -> Some(d, false)
                                  | HandedAt(callee, _, _) -> Some(Some callee, true)
                                  | _ -> None)

                          // every escape is an ownership transfer: nothing
                          // to say, a `use` here would be wrong
                          let transferred =
                              handedTo.IsNone && escapes |> List.exists (fun e -> e = Returned || e = Adopted)

                          // the `let` keyword: the LetOrUse node starts at it
                          let letRange =
                              Range.mkRange
                                  expr.Range.FileName
                                  expr.Range.Start
                                  (Position.mkPos expr.Range.StartLine (expr.Range.StartColumn + 3))

                          if textOfRange source letRange = "let" && not transferred then
                              let canFix = handedTo.IsNone && not inResult

                              { Range = letRange
                                Name = name
                                Fix = if canFix then Some("let", "use") else None
                                Destination = handedTo |> Option.bind fst
                                DestinationInspected = handedTo |> Option.exists snd
                                Context =
                                  match bindingContext declPath expr.Range with
                                  // a handle the OS reclaims at exit is no loss
                                  // in main; unflushed work is
                                  | Some ScopeContext.EntryPoint when not (flushSensitive check source binder) -> None
                                  | context -> context }
                  | _ -> ()
              | _ -> () ]
