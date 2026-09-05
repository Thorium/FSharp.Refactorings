/// Refactoring note (correctness): a catch-all handler that does nothing —
/// or quietly substitutes a default value — swallows every exception.
///
///     try work () with _ -> ()              // hides bugs AND cancellation
///     try work () with :? Exception -> ()
///     try read () with _ -> ""              // masks failure as an answer
///     try count () with _ -> 0
///     try get () with _ -> Unchecked.defaultof<_>
///
/// An empty catch of System.Exception silently eats programming errors,
/// OperationCanceledException, and everything else; a default-value catch
/// additionally disguises the failure as a legitimate result. Advice: the
/// best fix is usually no catch at all — a guard on the value that would
/// throw — then a specific exception type, then at least a log line.
///
/// The editor offers, where the shape allows:
///   - the GUARD, for a body that is pure arithmetic with one division by
///     a non-literal: `if x = 0 then fallback else a / x` — the catch is
///     removable because nothing else in the body throws
///   - TryParse, for a body that is one `Int32.Parse s`-style call:
///     `match Int32.TryParse s with | true, v -> Some v | _ -> None`
///   - a NARROWER catch, for a body doing file IO: `:? IOException |
///     :? UnauthorizedAccessException` instead of everything
///   - a LOG LINE in the file's own logging idiom (Microsoft.Extensions
///     .Logging, Serilog or Logary, whichever the file already uses),
///     naming the exception, the method and its parameters, as the
///     handler's first statement
/// A sweep only notes: whether the catch can go is the author's call.
///
/// Only trivially empty or constant-default bodies with catch-all patterns
/// (`_`, a bare binder, or `:? System.Exception`) are flagged; a handler
/// that catches a SPECIFIC exception type and deliberately ignores it is a
/// decision, not an accident, and stays quiet.
module FSharp.Refactor.SwallowedException

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

/// One editor offer: what it does, and its edits.
type Offer =
    { Label: string
      Edits: (range * string * string) list }

type Suggestion =
    {
        Range: range
        /// The handler pattern's text, for the message.
        PatternText: string
        /// The substituted default's text (`""`, `0`, `Unchecked.defaultof`),
        /// or None for an empty `()` body.
        FallbackText: string option
        /// The editor's offers, best first.
        Offers: Offer list
    }

/// A pattern that matches every exception.
let private isCatchAll (pat: SynPat) =
    match pat with
    | SynPat.Wild _
    | SynPat.Named _ -> true
    | SynPat.IsInst(SynType.LongIdent(SynLongIdent(id = ids)), _) ->
        not ids.IsEmpty && (List.last ids).idText = "Exception"
    | SynPat.As(lhsPat = SynPat.IsInst(SynType.LongIdent(SynLongIdent(id = ids)), _)) ->
        not ids.IsEmpty && (List.last ids).idText = "Exception"
    | _ -> false

/// The name a catch-all binds the exception to, if any.
let private binderOf (pat: SynPat) =
    match pat with
    | SynPat.Named(ident = SynIdent(ident = id)) -> Some id.idText
    | SynPat.As(rhsPat = SynPat.Named(ident = SynIdent(ident = id))) -> Some id.idText
    | _ -> None

/// A body that substitutes a default-ish value for the exception: a bare
/// constant, `Unchecked.defaultof<_>`, None/ValueNone, or an empty
/// collection literal.
let private isDefaultFallback (e: SynExpr) =
    match e with
    | SynExpr.Const(SynConst.Unit, _) -> false // handled as the empty body
    // bools are decided by the caller, which can see the try BODY: the
    // `try ping (); true with _ -> false` probe stays quiet, while
    // `try parse s with _ -> false` disguises the failure as an answer
    | SynExpr.Const(SynConst.Bool _, _) -> false
    | SynExpr.Const _ -> true
    | SynExpr.Null _ -> true
    | IdentName("None" | "ValueNone") -> true
    | SynExpr.ArrayOrList(_, [], _) -> true
    // dotted defaults: String.Empty, DateTime.MinValue, TimeSpan.Zero,
    // Array.empty, Map.empty, ...
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
        not ids.IsEmpty
        && (match (List.last ids).idText with
            | "Empty"
            | "empty"
            | "MinValue"
            | "Zero"
            | "Default" -> true
            | _ -> false)
        ->
        true
    | SynExpr.TypeApp(expr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))) ->
        not ids.IsEmpty && (List.last ids).idText = "defaultof"
    | _ -> false

[<return: Struct>]
let inline private (|IsDefaultFallback|_|) input =
    if isDefaultFallback input then
        ValueSome input
    else
        ValueNone

/// The expression a block evaluates to — the tail of its Sequential chain.
[<TailCall>]
let rec private lastExprOf (e: SynExpr) =
    match e with
    | SynExpr.Sequential(expr2 = e2) -> lastExprOf e2
    | SynExpr.Paren(expr = inner) -> lastExprOf inner
    | _ -> e

/// Is a bool-literal catch-all the PROBE idiom — the try body answering
/// with the opposite literal (`try ping (); true with _ -> false`, or the
/// inverted did-it-throw probe)? Then the failure IS the answer. Any other
/// body makes the literal a disguised default like the rest.
let private isBoolProbe (tryBody: SynExpr) (fallback: bool) =
    match lastExprOf tryBody with
    | SynExpr.Const(SynConst.Bool bodyValue, _) -> bodyValue <> fallback
    | _ -> false

// ---- the offers ----

let private arithmeticOps =
    set
        [ "op_Addition"
          "op_Subtraction"
          "op_Multiply"
          "op_Division"
          "op_Modulus"
          "op_UnaryNegation" ]

/// Is the expression arithmetic over names and literals only — nothing
/// that can throw but a division? Returns the non-literal divisors.
let rec private pureArithmetic (e: SynExpr) : (bool * SynExpr list) =
    match e with
    | SynExpr.Paren(expr = inner) -> pureArithmetic inner
    | SynExpr.Const(SynConst.Unit, _) -> false, []
    | SynExpr.Const _
    | SynExpr.Ident _
    | SynExpr.LongIdent _ -> true, []
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = SingleIdent op; argExpr = l); argExpr = r) when
        arithmeticOps.Contains op.idText
        ->
        let okL, dl = pureArithmetic l
        let okR, dr = pureArithmetic r

        let divisor =
            match op.idText, stripParens r with
            | ("op_Division" | "op_Modulus"), SynExpr.Const _ -> []
            | ("op_Division" | "op_Modulus"), d -> [ d ]
            | _ -> []

        okL && okR, dl @ dr @ divisor
    | SynExpr.App(funcExpr = SingleIdent op; argExpr = inner) when op.idText = "op_UnaryNegation" ->
        pureArithmetic inner
    | _ -> false, []

let private parseTypes =
    set
        [ "Int32"
          "Int64"
          "Int16"
          "Byte"
          "UInt32"
          "UInt64"
          "Double"
          "Single"
          "Decimal"
          "DateTime"
          "DateTimeOffset"
          "TimeSpan"
          "Guid"
          "Boolean" ]

/// `T.Parse arg` / `T.Parse(arg)` with one argument.
let private parseCall (e: SynExpr) =
    match stripParens e with
    | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)); argExpr = arg) when
        ids.Length >= 2
        && (List.last ids).idText = "Parse"
        && parseTypes.Contains ids.[ids.Length - 2].idText
        ->
        match stripParens arg with
        | SynExpr.Tuple _ -> None
        | single -> Some(ids |> List.take (ids.Length - 1) |> identText, single)
    | _ -> None

let private ioSmell =
    System.Text.RegularExpressions.Regex(
        @"\b(File|Directory|Path|FileInfo|DirectoryInfo|FileStream|StreamReader|StreamWriter|BinaryReader|BinaryWriter)\b",
        System.Text.RegularExpressions.RegexOptions.Compiled
    )

/// The logging idiom this file already uses: the receiver of an MEL call
/// (`logger`), Serilog's static `Log`, or a Logary pipeline's sink text.
type private LogIdiom =
    | Mel of receiver: string
    | Serilog
    | Logary of sink: string

let private melMethods =
    set
        [ "LogError"
          "LogWarning"
          "LogInformation"
          "LogDebug"
          "LogCritical"
          "LogTrace" ]

let private serilogMethods =
    set [ "Error"; "Warning"; "Information"; "Debug"; "Fatal"; "Verbose" ]

let private logIdiomOf (index: AstIndex.Index) (source: ISourceText) =
    let rec stages (e: SynExpr) =
        match e with
        | SynExpr.App(
            isInfix = false
            funcExpr = SynExpr.App(isInfix = true; funcExpr = SingleIdent op; argExpr = left)
            argExpr = right) when op.idText = "op_PipeRight" -> stages left @ [ right ]
        | other -> [ other ]

    index.Exprs
    |> Array.tryPick (fun (_, e) ->
        match e with
        | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))) when
            ids.Length >= 2 && melMethods.Contains (List.last ids).idText
            ->
            Some(Mel(ids |> List.take (ids.Length - 1) |> identText))
        | SynExpr.App(isInfix = false; funcExpr = SynExpr.DotGet(expr = recv; longDotId = SynLongIdent(id = [ m ]))) when
            melMethods.Contains m.idText
            ->
            Some(Mel(textOfRange source recv.Range))
        | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = [ l; m ]))) when
            l.idText = "Log" && serilogMethods.Contains m.idText
            ->
            Some Serilog
        | SynExpr.App(isInfix = false; funcExpr = SynExpr.App(isInfix = true; funcExpr = SingleIdent op)) when
            op.idText = "op_PipeRight"
            ->
            let chain = stages e

            let isLogaryEvent (s: SynExpr) =
                match s with
                | SynExpr.App(funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)))
                | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) ->
                    ids.Length >= 2
                    && ids.[ids.Length - 2].idText = "Message"
                    && (List.last ids).idText.StartsWith "event"
                | _ -> false

            if chain |> List.exists isLogaryEvent then
                Some(Logary(textOfRange source (List.last chain).Range))
            else
                None
        | _ -> None)

/// The enclosing binding's name and parameter names, from the path.
let private enclosingFunction (path: SyntaxNode list) =
    path
    |> List.tryPick (fun node ->
        match node with
        | SyntaxNode.SynBinding(SynBinding(
            headPat = SynPat.LongIdent(longDotId = SynLongIdent(id = ids); argPats = SynArgPats.Pats args))) when
            not ids.IsEmpty
            ->
            Some((List.last ids).idText, args |> List.collect patBoundNames)
        | SyntaxNode.SynBinding(SynBinding(headPat = SynPat.Named(ident = SynIdent(ident = id)))) ->
            Some(id.idText, [])
        | _ -> None)

/// The log statement, in the idiom, for an exception bound to `ex`.
let private logLine (idiom: LogIdiom) (ex: string) (method': string) (parameters: string list) =
    let placeholders =
        match parameters with
        | [] -> ""
        | [ p ] -> $" with parameter {{{p}}}"
        | ps ->
            " with parameters "
            + (ps |> List.map (fun p -> "{" + p + "}") |> String.concat " ")

    let template = $"Exception: {{Message}} in method {{Method}}{placeholders}"

    match idiom with
    | Mel receiver ->
        let args = [ $"{ex}.Message"; $"\"{method'}\"" ] @ parameters |> String.concat ", "
        $"{receiver}.LogError({ex}, \"{template}\", {args})"
    | Serilog ->
        let args = [ $"{ex}.Message"; $"\"{method'}\"" ] @ parameters |> String.concat ", "
        $"Log.Error({ex}, \"{template}\", {args})"
    | Logary sink ->
        let fields =
            [ $"Message\" {ex}.Message"; $"Method\" \"{method'}\"" ]
            @ (parameters |> List.map (fun p -> $"{p}\" {p}"))
            |> List.map (fun f -> $" |> Message.setField \"{f}")
            |> String.concat ""

        $"Message.eventError \"{template}\"{fields} |> Message.addExn {ex} |> {sink}"

/// The zero of a divisor's type, as F# spells it — `0`, `0L`, `0m` — from
/// the typed check; None when the type is unknown or has no literal zero,
/// and the guard is not offered. Floats are left out on purpose: float
/// division never throws (it yields infinity or NaN), so the catch was
/// never reached and a guard would change the result, not remove a catch.
let private zeroOf (check: FSharpCheckFileResults option) (source: ISourceText) (divisor: SynExpr) =
    let ident =
        match divisor with
        | SynExpr.Ident id -> Some id
        | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> Some(List.last ids)
        | _ -> None

    match check, ident with
    | Some check, Some id ->
        let r = id.idRange
        let lineText = source.GetLineString(r.EndLine - 1)

        let names =
            match divisor with
            | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) -> ids |> List.map (fun i -> i.idText)
            | _ -> [ id.idText ]

        match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, names) with
        | Some symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as v ->
                (try
                    let t = OptionModule.stripAbbreviations v.FullType

                    match
                        (if t.HasTypeDefinition then
                             t.TypeDefinition.TryFullName
                         else
                             None)
                    with
                    | Some "System.Int32" -> Some "0"
                    | Some "System.Int64" -> Some "0L"
                    | Some "System.Int16" -> Some "0s"
                    | Some "System.Byte" -> Some "0uy"
                    | Some "System.SByte" -> Some "0y"
                    | Some "System.UInt32" -> Some "0u"
                    | Some "System.UInt64" -> Some "0UL"
                    | Some "System.UInt16" -> Some "0us"
                    | Some "System.Decimal" -> Some "0m"
                    | _ -> None
                 with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                     None)
            | _ -> None
        | None -> None
    | _ -> None

/// The test frameworks whose files this rule leaves alone: a swallowed
/// exception in a test is a different habit from one in a service, and the
/// test runner reports the failure either way.
let private testFrameworkOpens =
    set
        [ "Xunit"
          "NUnit.Framework"
          "Expecto"
          "Microsoft.VisualStudio.TestTools.UnitTesting"
          "Fuchu"
          "TUnit" ]

let private isTestFile (index: AstIndex.Index) =
    index.Decls
    |> Array.exists (fun (_, d) ->
        match d with
        | SynModuleDecl.Open(target = SynOpenDeclTarget.ModuleOrNamespace(longId = SynLongIdent(id = ids))) ->
            testFrameworkOpens.Contains(ids |> List.map (fun i -> i.idText) |> String.concat ".")
        | _ -> false)

/// Find empty and default-substituting catch-all handlers.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults option) : Suggestion list =
    let index = AstIndex.ofTree parseTree
    let idiom = lazy (logIdiomOf index source)

    // a test file yields nothing to walk
    let exprs = if isTestFile index then [||] else index.Exprs

    [ for path, expr in exprs do
          match expr with
          | SynExpr.TryWith(tryExpr = tryBody; withCases = clauses) ->
              for clause in clauses do
                  match simpleClause clause with
                  | Some(pat, result) when isCatchAll pat ->
                      let fallback =
                          match stripParens result with
                          | UnitConst -> Some None
                          | SynExpr.Const(SynConst.Bool b, _) as body when not (isBoolProbe tryBody b) ->
                              Some(Some(textOfRange source body.Range))
                          | IsDefaultFallback body -> Some(Some(textOfRange source body.Range))
                          | _ -> None

                      match fallback with
                      | Some fallbackText ->
                          let bodyText = textOfRange source tryBody.Range
                          let patText = textOfRange source pat.Range

                          // 1. the guard: pure arithmetic, one non-literal
                          // divisor, nothing else that throws
                          let guard =
                              match fallbackText, pureArithmetic (stripParens tryBody) with
                              | Some fb, (true, [ divisor ]) when isSingleLine tryBody.Range ->
                                  // the zero is the divisor's own — 0, 0L,
                                  // 0.0, 0m — from the typed check; without
                                  // the type the guard is not offered
                                  match zeroOf check source divisor with
                                  | Some zero ->
                                      let d = textOfRange source divisor.Range

                                      [ { Label =
                                            $"Fix: guard the division instead of catching — `if {d} = {zero} then {fb} else ...`; the catch goes, nothing else in the body throws"
                                          Edits =
                                            [ expr.Range,
                                              textOfRange source expr.Range,
                                              $"if {d} = {zero} then {fb} else {bodyText}" ] } ]
                                  | None -> []
                              | _ -> []

                          // 2. TryParse, for a one-call Parse body
                          let tryParse =
                              match fallbackText, parseCall tryBody with
                              | Some fb, Some(typeName, arg) ->
                                  let a = textOfRange source arg.Range

                                  let a =
                                      match arg with
                                      | SynExpr.Ident _
                                      | SynExpr.Const _
                                      | SynExpr.LongIdent _ -> a
                                      | _ -> $"({a})"

                                  let success, failure =
                                      match fb with
                                      | "None" -> "Some v", "None"
                                      | "ValueNone" -> "ValueSome v", "ValueNone"
                                      | other -> "v", other

                                  let pad = String.replicate expr.Range.StartColumn " "

                                  [ { Label =
                                        $"Fix: {typeName}.TryParse instead of a catch — the parse failing is the expected case, not an exception"
                                      Edits =
                                        [ expr.Range,
                                          textOfRange source expr.Range,
                                          $"match {typeName}.TryParse {a} with\n{pad}| true, v -> {success}\n{pad}| _ -> {failure}" ] } ]
                              | _ -> []

                          // 3. a narrower catch for file IO
                          let narrower =
                              if ioSmell.IsMatch bodyText then
                                  let narrowed =
                                      match binderOf pat with
                                      | Some name ->
                                          $"(:? System.IO.IOException | :? System.UnauthorizedAccessException) as {name}"
                                      | None -> ":? System.IO.IOException | :? System.UnauthorizedAccessException"

                                  [ { Label =
                                        "Alternative: catch the IO exceptions only — IOException and UnauthorizedAccessException — and let the rest surface"
                                      Edits = [ pat.Range, patText, narrowed ] } ]
                              else
                                  []

                          // 4. a log line in the file's own idiom
                          let logging =
                              match idiom.Value with
                              | Some idiom ->
                                  let ex = binderOf pat |> Option.defaultValue "ex"

                                  let method', parameters = enclosingFunction path |> Option.defaultValue ("?", [])

                                  // the logger itself is not a parameter worth
                                  // logging
                                  let parameters =
                                      match idiom with
                                      | Mel receiver -> parameters |> List.filter (fun p -> p <> receiver)
                                      | Logary sink -> parameters |> List.filter (fun p -> not (sink.Contains p))
                                      | Serilog -> parameters

                                  let line = logLine idiom ex method' parameters
                                  let indent = String.replicate (clause.Range.StartColumn + 4) " "

                                  let bindEdit =
                                      match pat with
                                      | SynPat.Wild _ -> [ pat.Range, patText, ex ]
                                      | _ -> []

                                  let bodyEdit =
                                      match stripParens result with
                                      | UnitConst ->
                                          [ result.Range, textOfRange source result.Range, $"\n{indent}{line}" ]
                                      | _ ->
                                          [ result.Range,
                                            textOfRange source result.Range,
                                            $"\n{indent}{line}\n{indent}{textOfRange source result.Range}" ]

                                  [ { Label =
                                        $"Alternative: log it the way this file logs — the exception, the method '{method'}' and its parameters — before the fallback"
                                      Edits = bindEdit @ bodyEdit } ]
                              | None -> []

                          { Range = clause.Range
                            PatternText = patText
                            FallbackText = fallbackText
                            Offers = guard @ tryParse @ narrower @ logging }
                      | None -> ()
                  | _ -> ()
          | _ -> () ]
