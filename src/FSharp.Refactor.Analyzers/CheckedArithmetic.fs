/// Diagnostic (correctness): arithmetic on a near-limit integer constant.
///
///     let due = balance + 2_000_000_000       // int + int wraps SILENTLY
///
/// F# arithmetic is unchecked by default: an int overflow does not throw,
/// it wraps, and the corrupted value flows on — this very repository's
/// benchmark harness summed an array with `Array.sum` (wrapped, silently
/// wrong) and LINQ's checked `Sum` (threw) side by side before this rule
/// existed. A constant within a factor of two of the type's ceiling makes
/// overflow a plausibility, not a theory.
///
/// Advice only — the right repair is a judgment call:
///   - `open Microsoft.FSharp.Core.Operators.Checked` makes the scope's
///     operators throw OverflowException instead of wrapping
///   - a wider type (int64, bigint) removes the ceiling
///   - or the wraparound is intended, and saying so beats implying it
///
/// Guards: only decimal-spelled literals count — a hex constant
/// (0x7FFFFFFF) is a mask, not a magnitude; unsigned literals are skipped
/// (wraparound arithmetic on those is routinely deliberate); and a file
/// that already opens Checked operators has made its choice.
module FSharp.Refactor.CheckedArithmetic

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

type Suggestion =
    {
        Range: range
        /// The near-limit constant's source text, for the message.
        ConstantText: string
    }

/// int32 within a factor of two of the ceiling; int64 within a factor of
/// sixteen (ten-digit int64s are common as ids, eighteen-digit ones are
/// magnitudes).
let private nearLimit (c: SynConst) =
    match c with
    | SynConst.Int32 n -> n >= 1_073_741_824 || n <= -1_073_741_824
    | SynConst.Int64 n -> n >= 576_460_752_303_423_488L || n <= -576_460_752_303_423_488L
    | _ -> false

let private overflowOps = set [ "op_Addition"; "op_Multiply"; "op_Subtraction" ]

let find (parseTree: ParsedInput) (source: ISourceText) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    // a file that already chose checked operators needs no reminder
    let opensChecked =
        index.Decls
        |> Array.exists (fun (_, decl) ->
            match decl with
            | SynModuleDecl.Open(target = SynOpenDeclTarget.ModuleOrNamespace(longId = SynLongIdent(id = ids))) ->
                ids |> List.exists (fun id -> id.idText = "Checked")
            | _ -> false)

    if opensChecked then
        []
    else
        let nearLimitLiteral (e: SynExpr) =
            match stripParens e with
            | SynExpr.Const(constant = c) as constant when nearLimit c ->
                // decimal spellings only: hex is a mask, not a magnitude
                let text = textOfRange source constant.Range

                if text.StartsWith "0x" || text.StartsWith "0X" then
                    ValueNone
                else
                    ValueSome text
            | _ -> ValueNone

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.App(funcExpr = SynExpr.App(funcExpr = SingleIdent op; argExpr = lhs); argExpr = rhs) when
                  overflowOps.Contains op.idText
                  ->
                  match nearLimitLiteral lhs, nearLimitLiteral rhs with
                  | ValueSome text, _
                  | _, ValueSome text ->
                      { Range = expr.Range
                        ConstantText = text }
                  | _ -> ()
              | _ -> () ]
