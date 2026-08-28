module FSharp.Refactor.Tests.TaskStateMachineTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private adviceIn (source: string) =
    let tree, sourceText = parse source
    TaskStateMachine.find tree sourceText

/// n `let! xi = Task.FromResult i` lines, enough to cross the size gate.
let private awaits n =
    [ for i in 1..n -> sprintf "    let! x%d = System.Threading.Tasks.Task.FromResult %d" i i ]
    |> String.concat "\n"

[<Fact>]
let ``let rec inside a task is flagged regardless of size`` () =
    let suggestions =
        adviceIn
            "module Test\nlet f () = task {\n    let rec loop (n: int) = if n = 0 then 0 else loop (n - 1)\n    let! c = System.Threading.Tasks.Task.FromResult 3\n    return loop c\n}"

    match suggestions with
    | [ s ] -> Assert.Equal(TaskStateMachine.AdviceKind.HoistRecursiveFunction, s.Kind)
    | other -> failwithf "Expected exactly one let-rec advice, got %A" other

[<Fact>]
let ``let rec inside a nested lambda is not resumable code`` () =
    Assert.Empty(
        adviceIn
            "module Test\nlet f () = task {\n    let g = fun (n: int) -> (let rec loop m = if m = 0 then 0 else loop (m - 1) in loop n)\n    let! c = System.Threading.Tasks.Task.FromResult 3\n    return g c\n}"
    )

[<Fact>]
let ``leading plain lets in an oversized task are counted`` () =
    let suggestions =
        adviceIn (
            "module Test\nlet f () = task {\n    let a = 1\n    let b = 2\n"
            + awaits 8
            + "\n    return a + b + x1\n}"
        )

    match suggestions with
    | [ s ] -> Assert.Equal(TaskStateMachine.AdviceKind.HoistPlainLets 2, s.Kind)
    | other -> failwithf "Expected exactly one hoist advice, got %A" other

[<Fact>]
let ``oversized branching where both arms await suggests a split`` () =
    let source =
        "module Test\nlet f (cond: bool) = task {\n    if cond then\n"
        + awaits 4
        + "\n        return x1\n    else\n"
        + awaits 4
        + "\n        return x2\n}"

    // the awaits helper indents for a plain task body; re-indent for branches
    let source = source.Replace("    let!", "        let!")

    match adviceIn source with
    | [ s ] -> Assert.Equal(TaskStateMachine.AdviceKind.SplitBranches, s.Kind)
    | other -> failwithf "Expected exactly one split advice, got %A" other

[<Fact>]
let ``long tail after the last await in an oversized task suggests extraction`` () =
    let source =
        "module Test\nlet f () = task {\n"
        + awaits 8
        + "\n    let b = x1 + 1\n    let c = b * 2\n    let d = c - 3\n    let e = d + x2\n    return e\n}"

    match adviceIn source with
    | [ s ] ->
        match s.Kind with
        | TaskStateMachine.AdviceKind.ExtractTail lines -> Assert.True(lines >= 4)
        | other -> failwithf "Expected tail advice, got %A" other
    | other -> failwithf "Expected exactly one tail advice, got %A" other

[<Fact>]
let ``a lean task yields no advice`` () =
    Assert.Empty(
        adviceIn
            "module Test\nlet f () = task {\n    let a = 1\n    let! c = System.Threading.Tasks.Task.FromResult 3\n    return a + c\n}"
    )
