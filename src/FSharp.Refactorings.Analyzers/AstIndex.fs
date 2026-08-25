/// One shared traversal per parse tree. Every analyzer in this package used
/// to run its own full `walkAst`, so a keystroke in the editor paid for ~20
/// identical traversals. All analyzers receive the same ParsedInput instance
/// per file version, so the flattened node list is computed once and memoized
/// with a ConditionalWeakTable — it lives and dies with the tree.
///
/// `replay` feeds the memoized nodes into an ordinary SyntaxCollectorBase, so
/// analyzer code is unchanged: only expression and module-declaration
/// callbacks are used by this package's collectors.
module FSharp.Refactorings.AstIndex

open System.Runtime.CompilerServices
open FSharp.Compiler.Syntax
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting

type Index =
    { Exprs: (SyntaxNode list * SynExpr)[]
      Decls: (SyntaxNode list * SynModuleDecl)[] }

let private cache = ConditionalWeakTable<ParsedInput, Index>()

let private build (tree: ParsedInput) : Index =
    let exprs = ResizeArray()
    let decls = ResizeArray()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(path, expr) = exprs.Add(path, expr)
            override _.WalkSynModuleDecl(path, decl) = decls.Add(path, decl) }

    walkAst collector tree

    { Exprs = exprs.ToArray()
      Decls = decls.ToArray() }

/// The memoized flat node index for a parse tree.
let ofTree (tree: ParsedInput) : Index = cache.GetValue(tree, build)

/// Drive a collector from the memoized index instead of a fresh traversal.
let replay (collector: SyntaxCollectorBase) (tree: ParsedInput) : unit =
    let index = ofTree tree

    for path, expr in index.Exprs do
        collector.WalkExpr(path, expr)

    for path, decl in index.Decls do
        collector.WalkSynModuleDecl(path, decl)
