/// Refactoring (performance): give a trivial partial active pattern a struct
/// return, avoiding an option allocation on every match attempt.
///
///     let (|Even|_|) n =                      [<return: Struct>]
///         if n % 2 = 0 then Some n else None  let (|Even|_|) n =
///                                                 if n % 2 = 0 then ValueSome n else ValueNone
///
/// Call sites are unchanged — the attribute only changes the representation —
/// which is what makes this rewrite provably safe. Requires F# 6+.
///
/// Safety rules:
///   - module-level, single-binding partial active pattern `(|P|_|)`
///   - no existing attributes on the binding and no return-type annotation
///     (an `option` annotation would need to become `voption`)
///   - every result position in the body must be a literal `Some e` or `None`
///     (reached through if/match/let/sequential/parens); any other result
///     shape skips the suggestion
///   - `Some`/`None` must resolve to FSharp.Core's option cases
module FSharp.Refactorings.StructActivePattern

open System
open System.Text.RegularExpressions
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

/// A single text edit: range, original text, replacement text.
type Edit =
    { Range: range
      Original: string
      Replacement: string }

type Suggestion =
    {
        /// The active pattern's name text, e.g. "|Even|_|".
        PatternName: string
        /// Range of the pattern name, where the hint is anchored.
        NameRange: range
        /// The attribute insertion followed by one edit per Some/None token.
        Edits: Edit list
    }

/// Walk the result positions of the pending bodies, recording every literal
/// Some/None case token. Returns false if any result position has another
/// shape.
[<TailCall>]
let rec private collectResultsLoop (acc: ResizeArray<range * string>) (pending: SynExpr list) : bool =
    match pending with
    | [] -> true
    | e :: rest ->
        match e with
        | SynExpr.Paren(expr = inner) -> collectResultsLoop acc (inner :: rest)
        | SynExpr.App(funcExpr = SynExpr.Ident someIdent) when someIdent.idText = "Some" ->
            acc.Add(someIdent.idRange, "ValueSome")
            collectResultsLoop acc rest
        | SynExpr.Ident ident when ident.idText = "None" ->
            acc.Add(ident.idRange, "ValueNone")
            collectResultsLoop acc rest
        | SynExpr.IfThenElse(thenExpr = thenExpr; elseExpr = Some elseExpr) ->
            collectResultsLoop acc (thenExpr :: elseExpr :: rest)
        | SynExpr.Match(clauses = clauses) ->
            let results =
                clauses |> List.map (fun (SynMatchClause(resultExpr = result)) -> result)

            collectResultsLoop acc (results @ rest)
        | SynExpr.LetOrUse lou when not lou.IsBang -> collectResultsLoop acc (lou.Body :: rest)
        | SynExpr.Sequential(expr2 = expr2) -> collectResultsLoop acc (expr2 :: rest)
        | _ -> false

let private collectResults (acc: ResizeArray<range * string>) (e: SynExpr) : bool = collectResultsLoop acc [ e ]

let private isPartialActivePatternName (name: string) = Regex.IsMatch(name, @"^\|.+\|_\|$")

/// Find trivial partial active patterns that can get [<return: Struct>].
/// Requires typed check results for the Some/None gate.
let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkSynModuleDecl(_path, decl) =
                match decl with
                | SynModuleDecl.Let(
                    bindings = [ SynBinding(
                                     attributes = []
                                     returnInfo = None
                                     headPat = SynPat.LongIdent(longDotId = SynLongIdent(id = [ nameIdent ]))
                                     expr = body) ]) when isPartialActivePatternName nameIdent.idText ->
                    let results = ResizeArray<range * string>()

                    if collectResults results body && results.Count > 0 then
                        let indent = String(' ', decl.Range.StartColumn)

                        let insertEdit =
                            { Range = Range.mkRange decl.Range.FileName decl.Range.Start decl.Range.Start
                              Original = ""
                              Replacement = "[<return: Struct>]\n" + indent }

                        let tokenEdits =
                            results
                            |> Seq.map (fun (r, replacement) ->
                                { Range = r
                                  Original = textOfRange source r
                                  Replacement = replacement })
                            |> List.ofSeq

                        let gated =
                            tokenEdits
                            |> List.forall (fun e ->
                                let ident = Ident(e.Original, e.Range)

                                OptionModule.resolvesToCoreCase check source "Microsoft.FSharp.Core.Option<" ident)

                        if gated then
                            suggestions.Add
                                { PatternName = nameIdent.idText
                                  NameRange = nameIdent.idRange
                                  Edits = insertEdit :: tokenEdits }
                | _ -> () }

    if OptionModule.hasErrors check then
        []
    else
        AstIndex.replay collector parseTree
        List.ofSeq suggestions
