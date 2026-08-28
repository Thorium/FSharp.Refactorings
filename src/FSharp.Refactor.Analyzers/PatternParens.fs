/// Refactoring: drop parentheses a pattern does not need.
///
///     match x with | (Some y) -> ...   →  | Some y -> ...
///     match x with | (a, b) -> ...     →  | a, b -> ...
///     let f (x) = x                    →  let f x = x
///
/// Two shapes are safe, and only these:
///
///   1. the WHOLE pattern of a match or `with` clause. A clause pattern is
///      already delimited by `|` and `->`, so its outer parens never carry
///      meaning — whatever is inside them.
///
///   2. parens around a single ATOM anywhere else — a name, a wildcard, or a
///      non-negative constant. Anything richer may need them: `Some (x, y)`
///      is one tuple-carrying case and `Some x, y` is a pair, `f (x: int)`
///      annotates a parameter, and `Some (Some x)` would become the
///      nonsense `Some Some x`.
///
/// Member parameters are left alone even when atomic. `member _.M(x)` and
/// `member _.M x` agree for one parameter but disagree for none or several,
/// and a method's shape is not something a formatting fix should touch.
module FSharp.Refactor.PatternParens

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactor.Text

type Suggestion =
    {
        /// Range of the parenthesized pattern, i.e. the text the fix replaces.
        Range: range
        OriginalText: string
        ReplacementText: string
    }

/// A pattern that never needs parentheses around it.
let private isAtom (source: ISourceText) (pat: SynPat) =
    match pat with
    // `()` parses as parens around the unit constant, but they are the
    // pattern: `let f () = ...` defines a function and `let f = ...` a value
    | SynPat.Const(constant = SynConst.Unit) -> false
    | SynPat.Named _
    | SynPat.Wild _ -> true
    | SynPat.Const _ ->
        // a leading sign re-reads as an operator once the parens are gone
        let text = textOfRange source pat.Range
        not (text.StartsWith '-' || text.StartsWith '+')
    | _ -> false

/// Is this the entire pattern of a match/try-with clause, wrapping something
/// whose parens are pure noise?
///
/// The point of dropping parentheses is to remove punctuation a reader gains
/// nothing from — not to remove every pair the compiler tolerates. So this
/// covers a clause matching ONE thing, where the parens say nothing the `|`
/// and `->` do not:
///
///     | (Some y) ->     | (x) ->     | (_) ->     | (:? Foo) ->
///
/// A pattern with structure keeps them. `| (a, b) ->` groups a tuple at a
/// glance and `| a, b ->` makes the reader work it out, which is the opposite
/// of the point. A type annotation is not even optional —
/// `| request: HttpRequestMessage when ... ->` does not parse.
let private isWholeClausePattern (path: SyntaxNode list) (inner: SynPat) =
    let isOneThing =
        match inner with
        | SynPat.Named _
        | SynPat.Wild _
        | SynPat.Const _
        | SynPat.LongIdent _
        | SynPat.IsInst _ -> true
        | _ -> false

    match path with
    | SyntaxNode.SynMatchClause _ :: _ -> isOneThing
    | _ -> false

/// Does this pattern sit in a member's parameter list?
let private inMemberParameters (path: SyntaxNode list) =
    path
    |> List.exists (fun node ->
        match node with
        | SyntaxNode.SynMemberDefn _ -> true
        | _ -> false)

/// Find parenthesized patterns whose parens do nothing.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let index = AstIndex.ofTree parseTree
    let suggestions = ResizeArray<Suggestion>()

    for path, pat in index.Pats do
        match pat with
        | SynPat.Paren(pat = inner) when isSingleLine pat.Range ->
            let redundant =
                isWholeClausePattern path inner
                || (isAtom source inner && not (inMemberParameters path))

            if redundant then
                // The parens were also keeping this pattern apart from its
                // neighbours. `let f (a)(b: int)` bare is `a(b: int)`, an
                // application rather than two parameters, and `let f(x)`
                // bare is `let fx`. Where a paren was doing that work, a
                // space takes over.
                let line = source.GetLineString(pat.Range.StartLine - 1)

                let charBefore =
                    if pat.Range.StartColumn > 0 then
                        Some line.[pat.Range.StartColumn - 1]
                    else
                        None

                let charAfter =
                    if pat.Range.EndLine = pat.Range.StartLine && pat.Range.EndColumn < line.Length then
                        Some line.[pat.Range.EndColumn]
                    else
                        None

                let separates (c: char option) =
                    c |> Option.exists (Char.IsWhiteSpace >> not)

                let lead = if separates charBefore then " " else ""
                let trail = if separates charAfter then " " else ""

                suggestions.Add
                    { Range = pat.Range
                      OriginalText = textOfRange source pat.Range
                      ReplacementText = lead + textOfRange source inner.Range + trail }
        | _ -> ()

    List.ofSeq suggestions
