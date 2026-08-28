/// Refactoring: extract a `when` guard into a partial active pattern.
///
///     match x with                       let private (|IsEven|_|) input =
///     | n when isEven n -> ...      →        if isEven input then Some input else None
///     | n -> ...                         ...
///                                        match x with
///                                        | IsEven n -> ...
///                                        | n -> ...
///
/// The active pattern is inserted immediately before the enclosing
/// module-level declaration, so the guard function must be in scope there.
///
/// Safety rules:
///   - the guard must be exactly `f var` where `var` is the clause's bound
///     variable and `f` is a plain identifier or dotted path
///   - a single lowercase identifier `f` is rejected when the enclosing
///     declaration appears to bind it locally (let/fun/for/pattern binding —
///     checked conservatively on the declaration text), because the generated
///     binding would sit outside that scope; a dotted `Module.func` must
///     start with an uppercase segment
///   - skipped when the file already contains an active pattern of the same
///     name
module FSharp.Refactor.ActivePattern

open System
open System.Text.RegularExpressions
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Refactor.Text

type Suggestion =
    {
        /// The generated active-pattern name, e.g. "IsEven".
        PatternName: string
        /// Zero-width range at the insertion point (start of the enclosing declaration).
        InsertRange: range
        /// Text to insert: the pattern binding, a newline, and re-indentation.
        InsertText: string
        /// Range of `var when guard` inside the clause.
        ClauseRange: range
        OriginalClauseText: string
        /// Replacement, e.g. "IsEven n".
        ClauseText: string
    }

[<return: Struct>]
let private (|GuardFunction|_|) (e: SynExpr) =
    match e with
    | SynExpr.Ident ident when ident.idText.Length > 0 && Char.IsLower ident.idText.[0] ->
        ValueSome(ident.idText, e, false)
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when
        ids.Length >= 2
        && (List.head ids).idText.Length > 0
        && Char.IsUpper (List.head ids).idText.[0]
        ->
        ValueSome((List.last ids).idText, e, true)
    | _ -> ValueNone

/// Conservative check for the enclosing declaration locally binding `name`:
/// let/use bindings, lambda parameters, for-loop variables, or a match
/// pattern binding it directly after `|`.
let private locallyBound (declText: string) (name: string) =
    let n = Regex.Escape name

    [ @"let[^\n=]*\b" + n + @"\b"
      @"use[^\n=]*\b" + n + @"\b"
      @"fun[^\n>]*\b" + n + @"\b"
      @"for\s+" + n + @"\b"
      @"\|\s*" + n + @"\b\s*(->|when)" ]
    |> List.exists (fun pattern -> Regex.IsMatch(declText, pattern))

let private capitalize (name: string) =
    string (Char.ToUpperInvariant name.[0]) + name.Substring 1

/// Find `when` guards of the form `f var` that can become active patterns.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let suggestions = ResizeArray<Suggestion>()

    // only consulted when a candidate guard is found, which is rare
    let fileText =
        lazy
            ([ for i in 0 .. source.GetLineCount() - 1 -> source.GetLineString i ]
             |> String.concat "\n")

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(path, expr) =
                match expr with
                | SynExpr.Match(clauses = clauses) ->
                    // only plain module-level lets: inserting before a type
                    // declaration (a guard inside a member) would reference
                    // member parameters that are out of scope there, and a
                    // `let` directly under a namespace is not even legal
                    let enclosingDecl =
                        path
                        |> List.tryPick (fun node ->
                            match node with
                            | SyntaxNode.SynModule decl -> Some decl
                            | _ -> None)
                        |> Option.bind (fun decl ->
                            match decl with
                            | SynModuleDecl.Let _ -> Some decl
                            | _ -> None)

                    match enclosingDecl with
                    | None -> ()
                    | Some decl ->
                        // extracted only when a candidate clause is found
                        let declText = lazy (textOfRange source decl.Range)

                        for SynMatchClause(pat = pat; whenExpr = whenExpr) in clauses do
                            match pat, whenExpr with
                            | SynPat.Named(ident = SynIdent(ident = var)),
                              Some(SynExpr.App(
                                  funcExpr = GuardFunction(fnName, fnExpr, isDotted); argExpr = SynExpr.Ident arg) as guard) when
                                arg.idText = var.idText
                                && isSingleLine guard.Range
                                && (isDotted || not (locallyBound declText.Value fnName))
                                ->
                                let patternName = capitalize fnName

                                if not (fileText.Value.Contains $"(|{patternName}|") then
                                    let fnText = textOfRange source fnExpr.Range
                                    let indent = String(' ', decl.Range.StartColumn)

                                    // new code gets the best form directly:
                                    // private (no new API surface), inline
                                    // (tiny body, FS1113-safe because the
                                    // pattern is as private as any guard it
                                    // references), and struct-returning (no
                                    // allocation per match attempt)
                                    let binding =
                                        sprintf
                                            "[<return: Struct>]\n%slet inline private (|%s|_|) input =\n%s    if %s input then ValueSome input else ValueNone"
                                            indent
                                            patternName
                                            indent
                                            fnText

                                    let insertAt = Range.mkRange decl.Range.FileName decl.Range.Start decl.Range.Start

                                    let clauseRange = Range.mkRange pat.Range.FileName pat.Range.Start guard.Range.End

                                    suggestions.Add
                                        { PatternName = patternName
                                          InsertRange = insertAt
                                          InsertText = $"{binding}\n{indent}"
                                          ClauseRange = clauseRange
                                          OriginalClauseText = textOfRange source clauseRange
                                          ClauseText = $"{patternName} {var.idText}" }
                            | _ -> ()
                | _ -> () }

    AstIndex.replay collector parseTree
    // two guards using the same function would both insert the same pattern
    // definition; keep only the first suggestion per generated name
    suggestions |> Seq.distinctBy (fun s -> s.PatternName) |> List.ofSeq
