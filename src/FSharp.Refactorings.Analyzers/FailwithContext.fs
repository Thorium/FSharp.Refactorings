/// Refactoring (FR0092): give a static `failwith` message the context of
/// the call that produced it.
///
///     let mymethod x =                 let mymethod x =
///         failwith "Error"        →        failwith $"Error, calling mymethod with x: {x}"
///
/// A constant error string is the anti-pattern: every occurrence in the log
/// reads the same, so it says which line threw but nothing about why. The
/// enclosing function's arguments are the cheapest context available, and
/// they are in scope right there.
///
/// Only STATIC strings are rewritten. A message that is already
/// interpolated has had thought put into it, and amending it would fight
/// the author rather than help them.
///
/// Safety rules:
///   - `failwith` must resolve to FSharp.Core (not a local shadow)
///   - the argument is a single-line regular string literal: no verbatim or
///     triple-quoted strings (different escaping), and no `{`, `}` or `%`,
///     which change meaning once the string becomes interpolated
///   - the enclosing binding is a function with 1-4 parameters, all plain
///     names (`x`, `(x: int)`); wildcards, tuples and unit carry nothing to
///     report
///   - the message must not already mention a parameter by name — that is
///     a hand-written contextual message
///
/// The rewrite only changes the text of the exception; the exception type
/// and control flow are untouched. It does put argument VALUES into the
/// message, so the hint says so: on a parameter holding a secret or
/// personal data that is a logging decision, not a mechanical one.
module FSharp.Refactorings.FailwithContext

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactorings.Text

type Suggestion =
    {
        Range: range
        OriginalText: string
        ReplacementText: string
        /// The enclosing function, for the message.
        FunctionName: string
    }

/// A parameter that can be reported by name.
let private paramName (p: SynPat) =
    match p with
    | SynPat.Named(ident = SynIdent(ident = id)) -> Some id.idText
    | SynPat.Paren(pat = SynPat.Typed(pat = SynPat.Named(ident = SynIdent(ident = id)))) -> Some id.idText
    | SynPat.Typed(pat = SynPat.Named(ident = SynIdent(ident = id))) -> Some id.idText
    | _ -> None

/// A function binding we could name and quote parameters from: its whole
/// range, its name, and its parameter names.
let private describeBinding (binding: SynBinding) =
    match binding with
    | SynBinding(headPat = SynPat.LongIdent(longDotId = SynLongIdent(id = ids); argPats = SynArgPats.Pats pats)) when
        not (ids.IsEmpty || pats.IsEmpty) && pats.Length <= 4
        ->
        let names = pats |> List.map paramName

        if names |> List.forall Option.isSome then
            Some(binding.RangeOfBindingWithRhs, (List.last ids).idText, names |> List.choose id)
        else
            None
    | _ -> None

/// Find static failwith messages that can carry their caller's arguments.
/// Requires typed check results for the shadowing gate.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        // every function binding in the file, module-level and nested
        let functions = ResizeArray<range * string * string list>()

        for _, decl in index.Decls do
            match decl with
            | SynModuleDecl.Let(bindings = bindings) ->
                for binding in bindings do
                    describeBinding binding |> Option.iter functions.Add
            | _ -> ()

        for _, expr in index.Exprs do
            match expr with
            | SynExpr.LetOrUse lou ->
                for binding in lou.Bindings do
                    describeBinding binding |> Option.iter functions.Add
            | _ -> ()

        if functions.Count = 0 then
            []
        else
            [ for _, expr in index.Exprs do
                  match expr with
                  | SynExpr.App(
                      isInfix = false
                      funcExpr = SynExpr.Ident failwithIdent
                      argExpr = SynExpr.Const(SynConst.String(text = text; synStringKind = SynStringKind.Regular),
                                              literalRange)) when
                      failwithIdent.idText = "failwith"
                      && isSingleLine literalRange
                      && not (text.Contains '{' || text.Contains '}' || text.Contains '%')
                      ->
                      // the innermost enclosing function wins: it is the one
                      // whose arguments explain this particular throw
                      let enclosing =
                          functions
                          |> Seq.filter (fun (r, _, _) -> Range.rangeContainsRange r literalRange)
                          |> Seq.sortBy (fun (r, _, _) -> r.EndLine - r.StartLine)
                          |> Seq.tryHead

                      match enclosing with
                      | Some(_, functionName, paramNames) when
                          // a message already naming an argument was written
                          // deliberately
                          not (paramNames |> List.exists text.Contains)
                          && OptionModule.resolvesToCoreOperator check source failwithIdent
                          ->
                          let literalText = textOfRange source literalRange

                          if
                              literalText.StartsWith '"'
                              && literalText.EndsWith '"'
                              && not (literalText.StartsWith "\"\"\"")
                              && literalText.Length >= 2
                          then
                              let reported =
                                  paramNames |> List.map (fun p -> p + ": {" + p + "}") |> String.concat ", "

                              let suffix = $", calling {functionName} with {reported}"

                              { Range = literalRange
                                OriginalText = literalText
                                ReplacementText = "$" + literalText.Insert(literalText.Length - 1, suffix)
                                FunctionName = functionName }
                      | _ -> ()
                  | _ -> () ]
