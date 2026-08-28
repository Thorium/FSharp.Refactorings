/// Refactoring note (performance): a recursive function that re-enters
/// itself through a `seq { }` builds a nested enumerator per recursion
/// level.
///
///     let rec walk node = seq {
///         yield node.Value
///         for child in node.Children do
///             yield! walk child          // fresh enumerator chain per level
///     }
///
/// Every yielded element is then dragged through O(depth) MoveNext calls,
/// and each level allocates its own state machine — deep trees turn a
/// linear walk into a quadratic one. The remedy is one sequence with an
/// explicit stack:
///
///     let walk root = seq {
///         let stack = Stack [ root ]
///         while stack.Count > 0 do
///             let node = stack.Pop()
///             yield node.Value
///             for child in node.Children do stack.Push child
///     }
///
/// Advice only — the traversal order (and laziness granularity) is the
/// author's to preserve. Applies to `seq`, `taskSeq`, and `asyncSeq`
/// builders alike.
module FSharp.Refactor.RecursiveSeq

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        /// The self-referencing call site inside the seq body.
        Range: range
        /// The recursive function's name, for the message.
        FunctionName: string
        /// The builder used ("seq"/"taskSeq"/"asyncSeq").
        Builder: string
    }

let private seqBuilders = set [ "seq"; "taskSeq"; "asyncSeq" ]

/// The single name bound by a recursive binding's head pattern.
let private boundName (SynBinding(headPat = p)) =
    match p with
    | SynPat.LongIdent(longDotId = SynLongIdent(id = [ f ])) -> Some f.idText
    | SynPat.Named(ident = SynIdent(ident = f)) -> Some f.idText
    | _ -> None

/// Find recursive self-entries inside sequence builders.
let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    ignore source
    let index = AstIndex.ofTree parseTree

    // (function name, binding body range) for every recursive binding
    let recBindings =
        let fromBindings isRec bindings =
            if isRec then
                bindings
                |> List.choose (fun (SynBinding(expr = body) as binding) ->
                    boundName binding |> Option.map (fun name -> name, body.Range))
            else
                []

        let fromDecls =
            index.Decls
            |> Array.toList
            |> List.collect (fun (_, decl) ->
                match decl with
                | SynModuleDecl.Let(isRecursive = isRec; bindings = bindings) -> fromBindings isRec bindings
                | _ -> [])

        let fromExprs =
            index.Exprs
            |> Array.toList
            |> List.collect (fun (_, e) ->
                match e with
                | SynExpr.LetOrUse lou when lou.IsRecursive && not lou.IsBang -> fromBindings true lou.Bindings
                | _ -> [])

        fromDecls @ fromExprs

    // seq-builder CE bodies
    let seqBodies =
        index.Exprs
        |> Array.choose (fun (_, e) ->
            match e with
            | SynExpr.App(isInfix = false; funcExpr = IdentName builder; argExpr = SynExpr.ComputationExpr(expr = body)) when
                seqBuilders.Contains builder
                ->
                Some(builder, body.Range)
            | _ -> None)

    [ for name, bodyRange in recBindings do
          // seq bodies belonging to this binding
          let ownSeqs =
              seqBodies
              |> Array.filter (fun (_, seqRange) -> Range.rangeContainsRange bodyRange seqRange)

          // the first self-reference inside any of them
          let selfCall =
              index.Exprs
              |> Array.tryPick (fun (_, e) ->
                  match e with
                  | SynExpr.Ident id when id.idText = name ->
                      ownSeqs
                      |> Array.tryPick (fun (builder, seqRange) ->
                          if Range.rangeContainsRange seqRange id.idRange then
                              Some(id.idRange, builder)
                          else
                              None)
                  | _ -> None)

          match selfCall with
          | Some(callRange, builder) ->
              { Range = callRange
                FunctionName = name
                Builder = builder }
          | None -> () ]
