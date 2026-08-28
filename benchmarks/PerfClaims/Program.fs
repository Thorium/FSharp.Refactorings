/// Measured backing for the rule catalog's claims.
///
///     dotnet run -c Release --project benchmarks/PerfClaims
///
/// The contract per category:
///   - a PERFORMANCE rule's rewrite must be measurably faster, or
///     measurably allocation-free where allocation is the claim
///   - an IDIOM rule's rewrite must hold PARITY — same shape in nicer
///     clothes must never be slower (FR0050 once emitted `Seq.sum` for a
///     list, ~50% slower than the loop it replaced; that is how this file
///     came to exist)
///
/// Stopwatch loops, not BenchmarkDotNet — its generated host executables
/// tend to be blocked by application control on locked-down machines, and
/// category-level verdicts only need an order of magnitude. Run in
/// Release; treat differences under ~20% as noise.
module FSharp.Refactor.Benchmarks.PerfClaims

open System.Diagnostics

/// Time AND allocation per op: performance has two axes, and a rewrite
/// that holds the clock while adding garbage still loses under real GC
/// pressure — the microbenchmark clock hides what the collector pays
/// later (FR0011's Some-per-match is invisible in ns/op and very visible
/// in B/op).
let inline bench name iters ([<InlineIfLambda>] f: unit -> int) =
    // warmup, twice: tiering
    f () |> ignore
    f () |> ignore

    // best of three passes: single stopwatch loops at the ns scale swing
    // wildly with JIT tiering and alignment, and the MINIMUM is the run
    // least disturbed by either
    let mutable sink = 0L
    let mutable best = infinity
    let mutable bytesPerOp = 0.0

    for _ in 1..3 do
        let allocBefore = System.GC.GetAllocatedBytesForCurrentThread()
        let sw = Stopwatch.StartNew()

        for _ in 1..iters do
            sink <- sink + int64 (f ())

        sw.Stop()
        let allocAfter = System.GC.GetAllocatedBytesForCurrentThread()
        bytesPerOp <- float (allocAfter - allocBefore) / float iters
        best <- min best (float sw.Elapsed.TotalMilliseconds * 1e6 / float iters)

    printfn "  %-48s %10.1f ns/op %10.1f B/op" name best bytesPerOp
    sink

[<return: Struct>]
let (|EvenStruct|_|) (n: int) =
    if n % 2 = 0 then ValueSome n else ValueNone

let (|EvenRef|_|) (n: int) = if n % 2 = 0 then Some n else None

[<EntryPoint>]
let main _ =
    let mutable sink = 0L
    let xsList = List.init 1000 id
    let xsArr = Array.init 1000 id
    let sq = Seq.init 1000 id
    let opt = Some 42
    let s = "hello world, hello again"
    let idSet = Set.ofList xsList
    let queue = System.Collections.Concurrent.ConcurrentQueue<int>(Seq.init 1000 id)

    printfn "FR0002 (idiom, parity): match vs Option combinators"

    sink <-
        sink
        + bench "match Some v -> v+1 | None -> 0" 10_000_000 (fun () ->
            match opt with
            | Some v -> v + 1
            | None -> 0)

    sink <-
        sink
        + bench "Option.map (+1) |> Option.defaultValue 0" 10_000_000 (fun () ->
            opt |> Option.map (fun v -> v + 1) |> Option.defaultValue 0)

    printfn ""
    printfn "FR0050 (idiom, parity): sum of a 1000-int list"

    sink <-
        sink
        + bench "mutable for-loop" 100_000 (fun () ->
            let mutable t = 0

            for x in xsList do
                t <- t + x

            t)

    sink <- sink + bench "List.sum (what the fix emits)" 100_000 (fun () -> List.sum xsList)
    sink <- sink + bench "Seq.sum (what it must NOT emit for a list)" 100_000 (fun () -> Seq.sum xsList)

    printfn ""
    printfn "FR0101 (idiom, parity): index loop vs for-in on int[1000]"

    sink <-
        sink
        + bench "for i in 0..len-1, xs.[i]" 100_000 (fun () ->
            let mutable t = 0

            for i in 0 .. xsArr.Length - 1 do
                t <- t + xsArr.[i]

            t)

    sink <-
        sink
        + bench "for x in xs" 100_000 (fun () ->
            let mutable t = 0

            for x in xsArr do
                t <- t + x

            t)

    printfn ""
    printfn "FR0102 (performance): positional list indexing, 1000 accesses"

    sink <-
        sink
        + bench "xs.[i] per index (walks i conses)" 1_000 (fun () ->
            let mutable t = 0

            for i in 0..999 do
                t <- t + xsList.[i]

            t)

    sink <-
        sink
        + bench "for x in xs" 1_000 (fun () ->
            let mutable t = 0

            for x in xsList do
                t <- t + x

            t)

    printfn ""
    printfn "FR0035 (performance): 1000 membership probes"

    sink <-
        sink
        + bench "List.contains" 1_000 (fun () ->
            let mutable n = 0

            for i in 0..999 do
                if List.contains i xsList then
                    n <- n + 1

            n)

    sink <-
        sink
        + bench "Set.contains" 1_000 (fun () ->
            let mutable n = 0

            for i in 0..999 do
                if Set.contains i idSet then
                    n <- n + 1

            n)

    printfn ""
    printfn "FR0011 (performance, ALLOCATION is the claim): 1000 match attempts"

    sink <-
        sink
        + bench "reference-returning (Some per hit)" 100_000 (fun () ->
            let mutable n = 0

            for i in 0..999 do
                match i with
                | EvenRef _ -> n <- n + 1
                | _ -> ()

            n)

    sink <-
        sink
        + bench "[<return: Struct>] (ValueSome)" 100_000 (fun () ->
            let mutable n = 0

            for i in 0..999 do
                match i with
                | EvenStruct _ -> n <- n + 1
                | _ -> ()

            n)

    printfn ""
    printfn "FR0021 (performance): Contains string vs char"
    sink <- sink + bench "s.Contains \"o\"" 10_000_000 (fun () -> if s.Contains "o" then 1 else 0)
    sink <- sink + bench "s.Contains 'o'" 10_000_000 (fun () -> if s.Contains 'o' then 1 else 0)

    printfn ""
    printfn "FR0004 (performance): materialize-then-map vs map-then-materialize"

    sink <-
        sink
        + bench "Seq.toList |> List.map" 10_000 (fun () -> (sq |> Seq.toList |> List.map (fun x -> x + 1)).Length)

    sink <-
        sink
        + bench "Seq.map |> Seq.toList" 10_000 (fun () -> (sq |> Seq.map (fun x -> x + 1) |> Seq.toList).Length)

    printfn ""
    printfn "FR0052 (performance): ConcurrentQueue emptiness, n=1000"
    sink <- sink + bench "q.Count = 0" 1_000_000 (fun () -> if queue.Count = 0 then 1 else 0)
    sink <- sink + bench "q.IsEmpty" 1_000_000 (fun () -> if queue.IsEmpty then 1 else 0)

    printfn ""
    printfn "FR0064 (performance): int array sum, SIMD claim"
    sink <- sink + bench "Array.sum, n=1000" 100_000 (fun () -> Array.sum xsArr)
    sink <- sink + bench "Enumerable.Sum, n=1000" 100_000 (fun () -> System.Linq.Enumerable.Sum xsArr)
    // small values: Enumerable.Sum is CHECKED arithmetic, 0..99999 overflows
    let big = Array.init 100_000 (fun i -> i % 100)
    sink <- sink + bench "Array.sum, n=100k" 2_000 (fun () -> Array.sum big)
    sink <- sink + bench "Enumerable.Sum, n=100k" 2_000 (fun () -> System.Linq.Enumerable.Sum big)
    // Min has no overflow check in its SIMD path, unlike Sum
    sink <- sink + bench "Array.min, n=100k" 2_000 (fun () -> Array.min big)
    sink <- sink + bench "Enumerable.Min, n=100k" 2_000 (fun () -> System.Linq.Enumerable.Min big)

    printfn ""
    printfn "(sink %d)" (sink % 7L)
    0
