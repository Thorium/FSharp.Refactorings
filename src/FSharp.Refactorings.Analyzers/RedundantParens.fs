/// Refactoring: drop redundant parentheses around a single atomic argument.
///
///     List.max([4; 3])       →  List.max [4; 3]
///     String.length(s)       →  String.length s
///     f("literal")           →  f "literal"
///
/// Safety rules:
///   - the argument must be a single atom: an identifier, a dotted path, a
///     non-negative constant, or a collection literal — never a tuple (those
///     parens are a method-call argument list) and never an application
///   - the callee must be a plain identifier or an uppercase-headed dotted
///     path (module functions and static members); instance-method calls
///     keep their parens per the F# style guide
///   - calls continued by a projection (`f(x).Length`, `f(x).[0]`) are left
///     alone — the parens make the application atomic there
module FSharp.Refactorings.RedundantParens

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactorings.Text

type Suggestion =
    {
        /// Range of the parenthesized argument, i.e. the text the fix replaces.
        Range: range
        OriginalText: string
        ReplacementText: string
    }

/// A callee we are confident is a function or static member, not an instance
/// method: a bare identifier, or a dotted path whose head is uppercase.
[<return: Struct>]
let private (|FunctionCallee|_|) (e: SynExpr) =
    match e with
    | SynExpr.Ident _ -> ValueSome()
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = head :: _)) when
        head.idText.Length > 0 && Char.IsUpper head.idText.[0]
        ->
        ValueSome()
    | _ -> ValueNone

/// An argument that stays unambiguous without its parens.
let private isBareableArgument (source: ISourceText) (inner: SynExpr) =
    match inner with
    // operator references parse as idents whose range includes their own
    // parens (`(+)`), which splice fine; only a bare operator token would not
    | SynExpr.Ident _ ->
        let text = textOfRange source inner.Range

        text.StartsWith '('
        || (text.Length > 0 && (System.Char.IsLetter text.[0] || text.[0] = '_'))
    | SynExpr.LongIdent _ -> true
    | SynExpr.Const _ ->
        // a leading sign would re-parse as a binary operator: `f(-1)` / `f(+1)`
        let text = textOfRange source inner.Range
        not (text.StartsWith '-' || text.StartsWith '+')
    | SynExpr.ArrayOrList _
    | SynExpr.ArrayOrListComputed _ -> true
    | _ -> false

/// Find single-argument calls whose parenthesized argument is a bare atom.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(path, expr) =
                match expr with
                | SynExpr.App(
                    isInfix = false
                    funcExpr = (FunctionCallee as callee)
                    argExpr = (SynExpr.Paren(expr = inner; rightParenRange = Some _) as argExpr)) when
                    isSingleLine argExpr.Range && isBareableArgument source inner
                    ->
                    // `f(x).Length` needs the atomic application: skip under projections
                    let projected =
                        match path with
                        | SyntaxNode.SynExpr(SynExpr.DotGet _) :: _
                        | SyntaxNode.SynExpr(SynExpr.DotIndexedGet _) :: _ -> true
                        | _ -> false

                    if not projected then
                        let innerText = textOfRange source inner.Range

                        let replacement =
                            // `f(x)` has no space between callee and argument
                            if callee.Range.End = argExpr.Range.Start then
                                " " + innerText
                            else
                                innerText

                        suggestions.Add
                            { Range = argExpr.Range
                              OriginalText = textOfRange source argExpr.Range
                              ReplacementText = replacement }
                | _ -> () }

    AstIndex.replay collector parseTree
    List.ofSeq suggestions
