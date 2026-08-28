/// One shared traversal per parse tree. Every analyzer in this package used
/// to run its own full `walkAst`, so a keystroke in the editor paid for ~20
/// identical traversals. All analyzers receive the same ParsedInput instance
/// per file version, so the flattened node list is computed once and memoized
/// with a ConditionalWeakTable — it lives and dies with the tree.
///
/// `replay` feeds the memoized nodes into an ordinary SyntaxCollectorBase, so
/// analyzer code is unchanged. It drives exactly the callbacks a collector
/// overrides — every kind it asks for, and no kind it ignores.
module FSharp.Refactor.AstIndex

open System.Collections.Concurrent
open System.Runtime.CompilerServices
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Xml
open FSharp.Compiler.SyntaxTrivia
open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting

type Index =
    { Exprs: (SyntaxNode list * SynExpr)[]
      Decls: (SyntaxNode list * SynModuleDecl)[]
      Pats: (SyntaxNode list * SynPat)[]
      Types: (SyntaxNode list * SynType)[]
      Attributes: (SyntaxNode list * SynAttribute)[] }

let private cache = ConditionalWeakTable<ParsedInput, Index>()

/// Member bindings of an object expression. The SDK walker visits an
/// ObjExpr's `bindings` and each interface impl's `bindings`, but FCS 43
/// parses `member ...` definitions into the `members` fields it never
/// touches — without this, expressions inside `{ new IDisposable with
/// member _.Dispose() = ... }` are invisible to every analyzer.
let private objExprMemberBindings (e: SynExpr) : SynBinding list =
    let ofMembers (members: SynMemberDefns) =
        members
        |> List.collect (fun m ->
            match m with
            | SynMemberDefn.Member(memberDefn = binding) -> [ binding ]
            | SynMemberDefn.GetSetMember(memberDefnForGet = getB; memberDefnForSet = setB) ->
                List.choose id [ getB; setB ]
            | _ -> [])

    match e with
    | SynExpr.ObjExpr(members = members; extraImpls = impls) ->
        ofMembers members
        @ (impls
           |> List.collect (fun (SynInterfaceImpl(members = implMembers)) -> ofMembers implMembers))
    | _ -> []

/// Wrap bindings in a one-declaration synthetic file so the SDK walker can
/// traverse their bodies; only expressions are collected from it.
let private syntheticTree (bindings: SynBinding list) : ParsedInput =
    let decl = SynModuleDecl.Let(false, bindings, Range.range0, { InKeyword = None })

    let modOrNs =
        SynModuleOrNamespace(
            [ Ident("Synthetic", Range.range0) ],
            false,
            SynModuleOrNamespaceKind.NamedModule,
            [ decl ],
            PreXmlDoc.Empty,
            [],
            None,
            Range.range0,
            { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }
        )

    ParsedInput.ImplFile(
        ParsedImplFileInput(
            "synthetic.fs",
            false,
            QualifiedNameOfFile(Ident("Synthetic", Range.range0)),
            [],
            [ modOrNs ],
            (false, false),
            { ConditionalDirectives = []
              WarnDirectives = []
              CodeComments = [] },
            Set.empty
        )
    )

let private build (tree: ParsedInput) : Index =
    let exprs = ResizeArray()
    let decls = ResizeArray()
    let pats = ResizeArray()
    let types = ResizeArray()
    let attributes = ResizeArray()

    let collector =
        { new SyntaxCollectorBase() with
            override _.WalkExpr(path, expr) = exprs.Add(path, expr)
            override _.WalkSynModuleDecl(path, decl) = decls.Add(path, decl)
            override _.WalkPat(path, pat) = pats.Add(path, pat)
            override _.WalkType(path, synType) = types.Add(path, synType) }

    walkAst collector tree

    // the SDK walker's walkAttribute never invokes the WalkAttribute
    // callback (it only descends into the argument expression), so
    // attributes are collected from the declarations directly
    let addAttrs path (attrs: SynAttributes) =
        for attrList in attrs do
            attributes.AddRange(attrList.Attributes |> Seq.map (fun attr -> path, attr))

    let ofMembers path (members: SynMemberDefn list) =
        for m in members do
            match m with
            | SynMemberDefn.Member(memberDefn = SynBinding(attributes = attrs)) -> addAttrs path attrs
            | SynMemberDefn.GetSetMember(memberDefnForGet = g; memberDefnForSet = s) ->
                for SynBinding(attributes = attrs) in List.choose id [ g; s ] do
                    addAttrs path attrs
            | SynMemberDefn.LetBindings(bindings = bindings) ->
                for SynBinding(attributes = attrs) in bindings do
                    addAttrs path attrs
            | SynMemberDefn.AutoProperty(attributes = attrs) -> addAttrs path attrs
            | SynMemberDefn.AbstractSlot(slotSig = SynValSig(attributes = attrs)) -> addAttrs path attrs
            | SynMemberDefn.ValField(fieldInfo = SynField(attributes = attrs)) -> addAttrs path attrs
            | SynMemberDefn.ImplicitCtor(attributes = attrs) -> addAttrs path attrs
            | _ -> ()

    for path, decl in decls do
        let declPath = SyntaxNode.SynModule decl :: path

        match decl with
        | SynModuleDecl.Let(bindings = bindings) ->
            for SynBinding(attributes = attrs) in bindings do
                addAttrs declPath attrs
        | SynModuleDecl.Attributes(attributes = attrs) -> addAttrs declPath attrs
        | SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo(attributes = attrs)) -> addAttrs declPath attrs
        | SynModuleDecl.Exception(
            exnDefn = SynExceptionDefn(exnRepr = SynExceptionDefnRepr(attributes = attrs); members = members)) ->
            addAttrs declPath attrs
            ofMembers declPath members
        | SynModuleDecl.Types(typeDefns = defns) ->
            for SynTypeDefn(typeInfo = SynComponentInfo(attributes = attrs); typeRepr = repr; members = extra) in defns do
                addAttrs declPath attrs
                ofMembers declPath extra

                match repr with
                | SynTypeDefnRepr.ObjectModel(members = members) -> ofMembers declPath members
                | SynTypeDefnRepr.Simple(simpleRepr = simple) ->
                    match simple with
                    | SynTypeDefnSimpleRepr.Union(unionCases = cases) ->
                        for SynUnionCase(attributes = attrs) in cases do
                            addAttrs declPath attrs
                    | SynTypeDefnSimpleRepr.Record(recordFields = fields) ->
                        for SynField(attributes = attrs) in fields do
                            addAttrs declPath attrs
                    | SynTypeDefnSimpleRepr.Enum(cases = cases) ->
                        for SynEnumCase(attributes = attrs) in cases do
                            addAttrs declPath attrs
                    | _ -> ()
                | _ -> ()
        | _ -> ()

    // supplement: walk object-expression member bodies the SDK walker skips;
    // lifted results splice in under the ObjExpr node's own path, and newly
    // surfaced nested object expressions are processed in turn
    let pending = System.Collections.Generic.Queue(exprs)
    let supplemental = ResizeArray()
    let supplementalPats = ResizeArray()
    let supplementalTypes = ResizeArray()

    while pending.Count > 0 do
        let objPath, objExpr = pending.Dequeue()

        match objExprMemberBindings objExpr with
        | [] -> ()
        | bindings ->
            let liftedExprs = ResizeArray()
            let liftedPats = ResizeArray()
            let liftedTypes = ResizeArray()

            // Patterns and types as well as expressions. Lifting only the
            // expressions left every pattern and type rule blind inside an
            // object expression — which is how FR0099 came to strip a list
            // pattern's `;` in SQLProvider: the tokenizer saw the semicolon,
            // and the pattern that made it a separator was not in the index.
            let liftedCollector =
                { new SyntaxCollectorBase() with
                    override _.WalkExpr(path, expr) = liftedExprs.Add(path, expr)
                    override _.WalkPat(path, pat) = liftedPats.Add(path, pat)
                    override _.WalkType(path, synType) = liftedTypes.Add(path, synType) }

            walkAst liftedCollector (syntheticTree bindings)

            // drop the synthetic module scaffolding at the path's tail and
            // graft onto the real ObjExpr location
            let graft path =
                let real = SyntaxNode.SynExpr objExpr :: objPath

                match List.rev path with
                | SyntaxNode.SynModuleOrNamespace _ :: SyntaxNode.SynModule _ :: kept -> List.rev kept @ real
                | _ -> path @ real

            for path, expr in liftedExprs do
                let grafted = graft path
                supplemental.Add(grafted, expr)
                pending.Enqueue(grafted, expr)

            supplementalPats.AddRange(liftedPats |> Seq.map (fun (path, pat) -> graft path, pat))

            supplementalTypes.AddRange(liftedTypes |> Seq.map (fun (path, synType) -> graft path, synType))

    exprs.AddRange supplemental
    pats.AddRange supplementalPats
    types.AddRange supplementalTypes

    { Exprs = exprs.ToArray()
      Decls = decls.ToArray()
      Pats = pats.ToArray()
      Types = types.ToArray()
      Attributes = attributes.ToArray() }

/// The memoized flat node index for a parse tree.
let ofTree (tree: ParsedInput) : Index = cache.GetValue(tree, build)

/// Which `Walk*` members a collector actually overrides.
///
/// Every node kind the index holds must be replayed to whoever asked for it —
/// driving only expressions and module declarations once made a `WalkPat`
/// override dead code that still looked like working analysis, which is how
/// FR0099 came to protect list expressions and silently not list patterns.
/// But replaying a kind to a collector that ignores it is thousands of
/// virtual no-ops per file per rule, and most rules want expressions alone.
///
/// So ask the type once and remember. An object expression compiles to its
/// own class per site, so these keys are few and stable.
let private drives = ConcurrentDictionary<System.Type, bool * bool * bool * bool>()

let private drivenBy (collector: SyntaxCollectorBase) =
    drives.GetOrAdd(
        collector.GetType(),
        fun collectorType ->
            let overrides (name: string) =
                collectorType.GetMethods(
                    System.Reflection.BindingFlags.Instance
                    ||| System.Reflection.BindingFlags.Public
                    ||| System.Reflection.BindingFlags.NonPublic
                )
                |> Array.exists (fun m -> m.Name = name && m.DeclaringType <> typeof<SyntaxCollectorBase>)

            overrides "WalkExpr", overrides "WalkSynModuleDecl", overrides "WalkPat", overrides "WalkType"
    )

/// Drive a collector from the memoized index instead of a fresh traversal.
let replay (collector: SyntaxCollectorBase) (tree: ParsedInput) : unit =
    let index = ofTree tree
    let walksExprs, walksDecls, walksPats, walksTypes = drivenBy collector

    if walksExprs then
        for path, expr in index.Exprs do
            collector.WalkExpr(path, expr)

    if walksDecls then
        for path, decl in index.Decls do
            collector.WalkSynModuleDecl(path, decl)

    if walksPats then
        for path, pat in index.Pats do
            collector.WalkPat(path, pat)

    if walksTypes then
        for path, synType in index.Types do
            collector.WalkType(path, synType)
