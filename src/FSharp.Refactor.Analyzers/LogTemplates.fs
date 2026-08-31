/// FR0124: structured-log templates that lie (notes, correctness).
///
///     logger.LogInformation("user {User} did {Action}", user)
///                                       // 2 placeholders, 1 argument
///     logger.LogError($"failed for {user}")
///                                       // interpolation destroys the
///                                       // template: every message is a
///                                       // distinct event to the sink
///     logger.LogWarning("{Id} then {Id} again", a, b)
///                                       // duplicate placeholder name
///
/// The template sibling of FR0048 (String.Format). Placeholder syntax:
/// `{Name}`, `{@Name}`, `{$Name}`, `{Name:format}`, `{Name,align}`;
/// `{{`/`}}` are literal braces. A leading exception argument (FR0120's
/// output included) is skipped — the template is the first string
/// literal. Typed-gated to Microsoft.Extensions.Logging.
module FSharp.Refactor.LogTemplates

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

[<RequireQualifiedAccess>]
type TemplateProblem =
    | CountMismatch of placeholders: int * arguments: int
    | DuplicateName of name: string
    | Interpolated

type Suggestion =
    { Range: range
      Problem: TemplateProblem
      LogMethod: string }

let private logMethods =
    set [ "LogTrace"; "LogDebug"; "LogInformation"; "LogWarning"; "LogError"; "LogCritical" ]

/// Placeholder names of a message template, `{{`-escapes skipped.
let internal placeholdersOf (template: string) =
    let names = ResizeArray<string>()
    let mutable i = 0

    while i < template.Length do
        if i + 1 < template.Length && template.[i] = '{' && template.[i + 1] = '{' then
            i <- i + 2
        elif template.[i] = '{' then
            let close = template.IndexOf('}', i + 1)

            if close > i then
                let raw = template.Substring(i + 1, close - i - 1)
                let name = raw.TrimStart('@', '$')

                let name =
                    match name.IndexOfAny [| ':'; ',' |] with
                    | -1 -> name
                    | cut -> name.Substring(0, cut)

                if name.Length > 0 then
                    names.Add name

                i <- close + 1
            else
                i <- template.Length
        else
            i <- i + 1

    List.ofSeq names

[<return: Struct>]
let private (|CallIdent|_|) (e: SynExpr) =
    match e with
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> ValueSome(List.last ids)
    | SynExpr.DotGet(longDotId = SynLongIdent(id = ids)) when not ids.IsEmpty -> ValueSome(List.last ids)
    | _ -> ValueNone

let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        let isLoggerExtension (logId: Ident) =
            let r = logId.idRange
            let lineText = source.GetLineString(r.EndLine - 1)

            match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ logId.idText ]) with
            | Some symbolUse ->
                match symbolUse.Symbol with
                | :? FSharpMemberOrFunctionOrValue as mfv ->
                    (try
                        mfv.DeclaringEntity
                        |> Option.bind (fun e -> e.TryFullName)
                        |> Option.map (fun n -> n.StartsWith "Microsoft.Extensions.Logging")
                        |> Option.defaultValue false
                     with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                         false)
                | _ -> false
            | None -> false

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.App(isInfix = false; funcExpr = CallIdent logId; argExpr = SynExpr.Paren(expr = inner)) when
                  logMethods.Contains logId.idText
                  ->
                  let args =
                      match inner with
                      | SynExpr.Tuple(exprs = es) -> es
                      | single -> [ single ]

                  // the template is the first STRING argument; anything
                  // before it (the exception) is skipped, anything after it
                  // feeds the placeholders
                  let templateIndex =
                      args
                      |> List.tryFindIndex (fun a ->
                          match a with
                          | SynExpr.Const(SynConst.String _, _)
                          | SynExpr.InterpolatedString _ -> true
                          | _ -> false)

                  // one trailing IDENT argument may be the params ARRAY
                  // passed whole — its element count is invisible here, so
                  // any arity claim would be a guess
                  let isParamsArrayPassThrough (trailing: SynExpr list) =
                      match trailing with
                      | [ SynExpr.Ident argId ] ->
                          let r = argId.idRange
                          let lineText = source.GetLineString(r.EndLine - 1)

                          (match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ argId.idText ]) with
                           | Some symbolUse ->
                               match symbolUse.Symbol with
                               | :? FSharpMemberOrFunctionOrValue as v ->
                                   (try
                                       let t = v.FullType.Format symbolUse.DisplayContext
                                       t.EndsWith "[]" || t.EndsWith "array"
                                    with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                                        true)
                               | _ -> false
                           | None -> false)
                      | _ -> false

                  match templateIndex with
                  | Some ti when isLoggerExtension logId ->
                      match List.item ti args with
                      | SynExpr.InterpolatedString(contents = parts; range = r) when
                          parts
                          |> List.exists (fun p ->
                              match p with
                              | SynInterpolatedStringPart.FillExpr _ -> true
                              | SynInterpolatedStringPart.String _ -> false)
                          ->
                          // hole-free $"..." compiles to a constant — only
                          // actual interpolation destroys the template
                          { Range = r
                            Problem = TemplateProblem.Interpolated
                            LogMethod = logId.idText }
                      | SynExpr.Const(SynConst.String(template, _, _), r) ->
                          let names = placeholdersOf template
                          let argCount = args.Length - ti - 1

                          let duplicate =
                              names
                              |> List.groupBy id
                              |> List.tryPick (fun (name, hits) -> if hits.Length > 1 then Some name else None)

                          match duplicate with
                          | Some name ->
                              { Range = r
                                Problem = TemplateProblem.DuplicateName name
                                LogMethod = logId.idText }
                          | None ->
                              if
                                  names.Length <> argCount
                                  && not (isParamsArrayPassThrough (List.skip (ti + 1) args))
                              then
                                  { Range = r
                                    Problem = TemplateProblem.CountMismatch(names.Length, argCount)
                                    LogMethod = logId.idText }
                      | _ -> ()
                  | _ -> ()
              | _ -> () ]
