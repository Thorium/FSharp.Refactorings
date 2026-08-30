module FSharp.Refactor.Tests.DictTryGetTests

open Xunit
open FSharp.Refactor
open FSharp.Refactor.Tests.Parsing

let private findIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    DictTryGet.find tree sourceText checkResults

let private assertSingleSuggestion (source: string) (expectedReplacement: string) =
    match findIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one suggestion, got %d: %A" (List.length other) other

let private assertNoSuggestion (source: string) = Assert.Empty(findIn source)

[<Fact>]
let ``single-line contains-then-index becomes TryGetValue`` () =
    assertSingleSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k = if d.ContainsKey k then d.[k] else 0"
        "match d.TryGetValue k with | true, value -> value | _ -> 0"

[<Fact>]
let ``multi-line if becomes a three-line match`` () =
    assertSingleSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k =\n    if d.ContainsKey k then\n        d.[k] + 1\n    else\n        0"
        "match d.TryGetValue k with\n    | true, value -> value + 1\n    | _ -> 0"

[<Fact>]
let ``indexer inside a larger then-branch is substituted`` () =
    assertSingleSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k = if d.ContainsKey k then string d.[k] else \"?\""
        "match d.TryGetValue k with | true, value -> string value | _ -> \"?\""

[<Fact>]
let ``fsharp6 indexing syntax is substituted`` () =
    assertSingleSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k = if d.ContainsKey k then d[k] else 0"
        "match d.TryGetValue k with | true, value -> value | _ -> 0"

[<Fact>]
let ``dotted container path works`` () =
    assertSingleSuggestion
        "open System.Collections.Generic\ntype S = { Cache: Dictionary<string, int> }\nlet f (s: S) k = if s.Cache.ContainsKey k then s.Cache.[k] else 0"
        "match s.Cache.TryGetValue k with | true, value -> value | _ -> 0"

[<Fact>]
let ``concurrent dictionary is flagged as concurrent`` () =
    let source =
        "open System.Collections.Concurrent\nlet f (d: ConcurrentDictionary<string, int>) k = if d.ContainsKey k then d.[k] else 0"

    match findIn source with
    | [ s ] -> Assert.True s.Concurrent
    | other -> failwithf "Expected exactly one concurrent suggestion, got %A" other

[<Fact>]
let ``fsharp Map gets the TryFind option idiom`` () =
    assertSingleSuggestion
        "let f (m: Map<string, int>) k = if m.ContainsKey k then m.[k] else 0"
        "match m.TryFind k with | Some value -> value | None -> 0"

[<Fact>]
let ``then-branch without the indexer is not rewritten`` () =
    assertNoSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k = if d.ContainsKey k then 1 else 0"

[<Fact>]
let ``effectful key is not rewritten`` () =
    // the key expression was evaluated twice; collapsing to once could change behavior
    assertNoSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) (mk: unit -> string) = if d.ContainsKey (mk ()) then d.[mk ()] else 0"

[<Fact>]
let ``branch already using the name value is not rewritten`` () =
    assertNoSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k (value: int) = if d.ContainsKey k then d.[k] + value else 0"

[<Fact>]
let ``indexer with a different key is not substituted`` () =
    assertNoSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k j = if d.ContainsKey k then d.[j] else 0"
// ---- harder cases: intermediate statements, false positives, forbidden substitutions ----

[<Fact>]
let ``multi-statement then-branch is left alone`` () =
    // intermediate commands between the lookup and the use: conservative skip
    assertNoSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k =\n    if d.ContainsKey k then\n        let v = d.[k]\n        v * 2\n    else\n        0"

[<Fact>]
let ``compound condition is not rewritten`` () =
    // `d.ContainsKey k && other` cannot become a bare TryGetValue match
    assertNoSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k (go: bool) = if d.ContainsKey k && go then d.[k] else 0"

[<Fact>]
let ``then-branch that mutates the dictionary first is not rewritten`` () =
    // sequential branch: substituting would change what the indexer sees
    assertNoSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k = if d.ContainsKey k then (d.Remove k |> ignore; d.[k]) else 0"

[<Fact>]
let ``indexer text inside a string literal is not substituted`` () =
    assertSingleSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k = if d.ContainsKey k then sprintf \"d.[k]=%d\" d.[k] else \"?\""
        "match d.TryGetValue k with | true, value -> sprintf \"d.[k]=%d\" value | _ -> \"?\""

[<Fact>]
let ``shadowed container inside a lambda is not substituted`` () =
    // the lambda re-binds d; its d.[k] belongs to the shadow, so no rewrite
    assertNoSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) (d2: Dictionary<string, int>) k = if d.ContainsKey k then (fun (d: Dictionary<string, int>) -> d.[k]) d2 else 0"

[<Fact>]
let ``similarly named container is not confused`` () =
    assertNoSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) (dd: Dictionary<string, int>) k = if d.ContainsKey k then dd.[k] else 0"

// ---- FR0018 TryAdd ----

let private tryAddIn (source: string) =
    let tree, sourceText, checkResults = parseAndCheck source
    DictTryGet.findTryAdd tree sourceText checkResults

let private assertTryAdd (source: string) (expectedReplacement: string) =
    match tryAddIn source with
    | [ s ] ->
        Assert.Equal(expectedReplacement, s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")
    | other -> failwithf "Expected exactly one TryAdd suggestion, got %d: %A" (List.length other) other

[<Fact>]
let ``check-then-add on Dictionary becomes TryAdd`` () =
    assertTryAdd
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k (v: int) = if not (d.ContainsKey k) then d.[k] <- v"
        "d.TryAdd(k, v) |> ignore"

[<Fact>]
let ``check-then-add on ConcurrentDictionary is flagged as a race`` () =
    let source =
        "open System.Collections.Concurrent\nlet f (d: ConcurrentDictionary<string, int>) k (v: int) = if not (d.ContainsKey k) then d.[k] <- v"

    match tryAddIn source with
    | [ s ] ->
        Assert.True s.Concurrent
        Assert.Equal("d.TryAdd(k, v) |> ignore", s.ReplacementText)
    | other -> failwithf "Expected exactly one concurrent TryAdd suggestion, got %A" other

[<Fact>]
let ``fsharp6 index-set syntax is recognized`` () =
    assertTryAdd
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k (v: int) = if not (d.ContainsKey k) then d[k] <- v"
        "d.TryAdd(k, v) |> ignore"

[<Fact>]
let ``update without the not-guard is an overwrite and stays`` () =
    Assert.Empty(
        tryAddIn
            "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k (v: int) = if d.ContainsKey k then d.[k] <- v"
    )

[<Fact>]
let ``effectful value is not moved into TryAdd`` () =
    // TryAdd evaluates the value always; the original evaluated it only when absent
    Assert.Empty(
        tryAddIn
            "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k (mk: unit -> int) = if not (d.ContainsKey k) then d.[k] <- mk ()"
    )

[<Fact>]
let ``check-then-add with an else branch stays`` () =
    Assert.Empty(
        tryAddIn
            "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k (v: int) = if not (d.ContainsKey k) then d.[k] <- v else d.[k] <- v + 1"
    )

[<Fact>]
let ``mismatched key is not rewritten as TryAdd`` () =
    Assert.Empty(
        tryAddIn
            "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k j (v: int) = if not (d.ContainsKey k) then d.[j] <- v"
    )

[<Fact>]
let ``sorted dictionary lacks TryAdd and stays`` () =
    Assert.Empty(
        tryAddIn
            "open System.Collections.Generic\nlet f (d: SortedDictionary<string, int>) k (v: int) = if not (d.ContainsKey k) then d.[k] <- v"
    )

// ---- match-on-ContainsKey form (user request) ----

[<Fact>]
let ``match on ContainsKey with interpolated indexer becomes TryGetValue`` () =
    assertSingleSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) x = match d.ContainsKey x with | true -> $\"hello {d[x]}\" | false -> \"not\""
        "match d.TryGetValue x with | true, value -> $\"hello {value}\" | _ -> \"not\""

[<Fact>]
let ``match on ContainsKey with wildcard miss clause works`` () =
    assertSingleSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k = match d.ContainsKey k with | true -> d.[k] | _ -> 0"
        "match d.TryGetValue k with | true, value -> value | _ -> 0"

[<Fact>]
let ``match with false clause first swaps the branches`` () =
    assertSingleSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k = match d.ContainsKey k with | false -> 0 | true -> d.[k]"
        "match d.TryGetValue k with | true, value -> value | _ -> 0"

[<Fact>]
let ``match form on Map uses TryFind`` () =
    assertSingleSuggestion
        "let f (m: Map<string, int>) k = match m.ContainsKey k with | true -> m.[k] | false -> 0"
        "match m.TryFind k with | Some value -> value | None -> 0"

[<Fact>]
let ``match with a when-guard is not rewritten`` () =
    assertNoSuggestion
        "open System.Collections.Generic\nlet f (d: Dictionary<string, int>) k (go: bool) = match d.ContainsKey k with | true when go -> d.[k] | _ -> 0"

[<Fact>]
let ``an elif chain peels one TryGetValue level per pass`` () =
    // the outer if rewrites alone; the elif comes along VERBATIM as the
    // fallthrough arm with its elif spelled back to if, which the next
    // fix-then-reanalyze pass rewrites in turn
    let source =
        "module Test\nopen System.Collections.Generic\nlet f (mapped: Dictionary<string, obj>) =\n    if mapped.ContainsKey \"FragmentId\" then\n        Some(mapped.[\"FragmentId\"].ToString())\n    elif mapped.ContainsKey \"Id\" then\n        Some(mapped.[\"Id\"].ToString())\n    else None"

    match findIn source with
    | [ s ] ->
        Assert.Contains("match mapped.TryGetValue \"FragmentId\" with", s.ReplacementText)
        Assert.Contains("| true, value -> Some(value.ToString())", s.ReplacementText)
        Assert.Contains("if mapped.ContainsKey \"Id\"", s.ReplacementText)
        Assert.DoesNotContain("elif", s.ReplacementText)
        let patched = applyEdit source s.Range s.ReplacementText
        Assert.True(typechecksCleanly patched, $"Patched source does not typecheck:\n%s{patched}")

        // second pass over the patched text peels the next level
        match findIn patched with
        | [ s2 ] ->
            Assert.Contains("match mapped.TryGetValue \"Id\" with", s2.ReplacementText)
            let patched2 = applyEdit patched s2.Range s2.ReplacementText
            Assert.True(typechecksCleanly patched2, $"Second pass does not typecheck:\n%s{patched2}")
        | other -> failwithf "Expected the second level on pass two, got %A" other
    | other -> failwithf "Expected exactly one chain suggestion, got %A" other
