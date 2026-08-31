/// FR0121: wall-clock traps on servers.
///
/// 1. `DateTime.UtcNow.Date` (note, always): truncating a UTC instant to
///    a date crosses NOBODY's midnight — the cut point is a random time
///    of day in every user's timezone. `DateTime.Today` is the same bug
///    in local clothing: the SERVER's calendar date, which the end user
///    never knows. Which timezone's date was meant is unknowable, so
///    this stays advice.
///
/// 2. `DateTime.Now` as a complete expression: on a server the local
///    clock is a deployment accident; `DateTime.UtcNow` records an
///    instant. The rewrite is offered in EDITORS and applied by the CLI
///    only under `{ "FR0121": { "utcNow": 1 } }` — Fable and desktop
///    software legitimately want local time, so the default never
///    rewrites. `DateTime.Now.Date`-style continuations are excluded
///    from the fix entirely: swapping Now for UtcNow under a calendar
///    read CREATES bug 1.
///
/// Both shapes are typed-gated to System.DateTime/System.DateTimeOffset.
module FSharp.Refactor.DateTimeRules

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
type WallClockKind =
    /// UtcNow.Date / Today: a timezone-random calendar cut.
    | UtcDateCut of text: string
    /// A bare DateTime.Now: the opt-in UtcNow rewrite.
    | LocalNow

type Suggestion =
    {
        Range: range
        Kind: WallClockKind
        /// For LocalNow: the `Now` ident to rewrite to `UtcNow`.
        FixRange: range option
    }

let private entityOf (check: FSharpCheckFileResults) (source: ISourceText) (ident: Ident) =
    let r = ident.idRange
    let lineText = source.GetLineString(r.EndLine - 1)

    match check.GetSymbolUseAtLocation(r.EndLine, r.EndColumn, lineText, [ ident.idText ]) with
    | Some symbolUse ->
        match symbolUse.Symbol with
        | :? FSharpMemberOrFunctionOrValue as mfv ->
            (try
                mfv.DeclaringEntity
                |> Option.bind (fun e -> e.TryFullName)
                |> Option.defaultValue ""
             with _ -> // deliberate fail-safe probe; fsharpanalyzer: ignore-line FR0055
                 "")
        | _ -> ""
    | None -> ""

let private isDateTimeEntity (name: string) =
    name = "System.DateTime" || name = "System.DateTimeOffset"

let find (parseTree: ParsedInput) (source: ISourceText) (check: FSharpCheckFileResults) : Suggestion list =
    if OptionModule.hasErrors check then
        []
    else
        let index = AstIndex.ofTree parseTree

        [ for _, expr in index.Exprs do
              match expr with
              | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when ids.Length >= 2 ->
                  let names = ids |> List.map (fun i -> i.idText)

                  // ...UtcNow.Date ANYWHERE in the chain (`.AddDays` etc may
                  // follow the cut), and Today likewise
                  let utcDateAt =
                      names
                      |> List.pairwise
                      |> List.tryFindIndex (fun (a, b) -> a = "UtcNow" && b = "Date")

                  match utcDateAt with
                  | Some i when isDateTimeEntity (entityOf check source (List.item i ids)) ->
                      { Range = expr.Range
                        Kind = WallClockKind.UtcDateCut(String.concat "." names)
                        FixRange = None }
                  | _ ->
                      let todayAt = names |> List.tryFindIndex ((=) "Today")

                      match todayAt with
                      | Some i when
                          i > 0
                          && entityOf check source (List.item i ids) = "System.DateTime"
                          ->
                          { Range = expr.Range
                            Kind = WallClockKind.UtcDateCut(String.concat "." names)
                            FixRange = None }
                      | _ ->
                          // a COMPLETE DateTime.Now — nothing after Now, so
                          // the UtcNow rewrite cannot create a calendar bug.
                          // DateTimeOffset.Now stays quiet entirely: it
                          // CARRIES its offset, which is often the point
                          match List.rev names with
                          | "Now" :: _ ->
                              let nowId = List.last ids

                              if entityOf check source nowId = "System.DateTime" then
                                  { Range = expr.Range
                                    Kind = WallClockKind.LocalNow
                                    FixRange = Some nowId.idRange }
                          | _ -> ()
              | _ -> () ]
