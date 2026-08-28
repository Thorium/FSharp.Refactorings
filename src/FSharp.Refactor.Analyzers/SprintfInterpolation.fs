/// Refactoring: a fully applied sprintf is a typed interpolated string.
///
///     sprintf "asdf %s: %d" name count   →  $"asdf %s{name}: %d{count}"
///
/// The interpolation keeps every format specifier exactly as written, so
/// the output goes through the same printf formatting and is
/// byte-identical — the gain is reading the arguments in place.
///
/// Safety rules:
///   - the format is a regular single-line string literal with no `{`/`}`
///     (they would need escaping) and only value specifiers — `%a`/`%t`
///     (function-taking) and `*` widths leave the call alone
///   - sprintf is fully applied: one simple argument (identifier, dotted
///     path, or non-string constant) per specifier; partial applications
///     never match because their argument count differs
module FSharp.Refactor.SprintfInterpolation

open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    { Range: range
      OriginalText: string
      ReplacementText: string }

/// A `%` specifier in a printf format: flags, width, precision, type.
let private specifierRegex =
    Regex(@"%[-+0# ]*[0-9]*(\.[0-9]+)?[a-zA-Z*]", RegexOptions.Compiled)

/// Value-producing specifier type characters we can splice; `%a`/`%t`
/// take function arguments and `*` widths take an extra argument.
let private isValueSpecifier (c: char) = "sdiuxXobcfFeEgGMAO".Contains c

/// The sprintf application spine: the sprintf identifier and the
/// argument list.
[<TailCall>]
let rec private collectSpine (args: SynExpr list) (e: SynExpr) =
    match e with
    | SynExpr.App(isInfix = false; funcExpr = f; argExpr = a) -> collectSpine (a :: args) f
    | SingleIdent id when id.idText = "sprintf" -> ValueSome(id, args)
    | _ -> ValueNone

/// A simple argument that reads well inside `{...}` and cannot contain
/// braces or nested quotes.
let private simpleArg (e: SynExpr) =
    match e with
    | SynExpr.Ident _
    | SynExpr.LongIdent _ -> true
    | SynExpr.Const(SynConst.String _, _) -> false
    | SynExpr.Const _ -> true
    | _ -> false

/// Find fully applied simple sprintf calls. Requires typed check results
/// (`sprintf` itself must resolve to FSharp.Core, not a shadow).
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    [ for _, expr in index.Exprs do
          match expr with
          | SynExpr.App(isInfix = false) when isSingleLine expr.Range ->
              match collectSpine [] expr with
              | ValueSome(sprintfId, (SynExpr.Const(SynConst.String(_, SynStringKind.Regular, _), _) as fmtExpr :: args)) when
                  not args.IsEmpty
                  && args |> List.forall simpleArg
                  && OptionModule.resolvesToCoreOperator check source sprintfId
                  ->
                  let fmtSource = textOfRange source fmtExpr.Range
                  let fmt = fmtSource.Substring(1, fmtSource.Length - 2)

                  let specifiers =
                      specifierRegex.Matches fmt
                      |> Seq.filter (fun m ->
                          // an even run of % before the match means the
                          // leading % is itself escaped (%%)
                          let mutable run = 0
                          let mutable i = m.Index - 1

                          while i >= 0 && fmt.[i] = '%' do
                              run <- run + 1
                              i <- i - 1

                          run % 2 = 0)
                      |> List.ofSeq

                  let spliceable =
                      not (fmt.Contains '{')
                      && not (fmt.Contains '}')
                      && specifiers.Length = args.Length
                      && specifiers
                         |> List.forall (fun m -> isValueSpecifier fmt.[m.Index + m.Length - 1])

                  if spliceable then
                      let builder = System.Text.StringBuilder()
                      let mutable cursor = 0

                      for m, arg in List.zip specifiers args do
                          builder
                              .Append(fmt.Substring(cursor, m.Index - cursor))
                              .Append(m.Value)
                              .Append('{')
                              .Append(textOfRange source arg.Range)
                              .Append
                              '}'
                          |> ignore

                          cursor <- m.Index + m.Length

                      builder.Append(fmt.Substring cursor) |> ignore

                      { Range = expr.Range
                        OriginalText = textOfRange source expr.Range
                        ReplacementText = "$\"" + builder.ToString() + "\"" }
              | _ -> ()
          | _ -> () ]
