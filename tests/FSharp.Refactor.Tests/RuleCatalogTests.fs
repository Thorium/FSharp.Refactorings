module FSharp.Refactor.Tests.RuleCatalogTests

open System.Text.RegularExpressions
open Xunit
open FSharp.Refactor

/// Every code the README documents. That table is the user-facing list of
/// rules, so it is the right thing to hold the catalog against.
let private documentedCodes () =
    let readme =
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "README.md")
        |> System.IO.File.ReadAllText

    Regex.Matches(readme, @"^\| (FR\d{4}) \|", RegexOptions.Multiline)
    |> Seq.map (fun m -> m.Groups.[1].Value)
    |> Set.ofSeq

[<Fact>]
let ``every documented rule has a category`` () =
    let documented = documentedCodes ()
    Assert.NotEmpty documented
    let missing = Set.difference documented RuleCatalog.known

    Assert.True(
        Set.isEmpty missing,
        sprintf "These rules have no category, so they silently read as idiom: %s" (String.concat ", " missing)
    )

[<Fact>]
let ``the catalog invents no rules`` () =
    let unknown = Set.difference RuleCatalog.known (documentedCodes ())

    Assert.True(
        Set.isEmpty unknown,
        sprintf "The catalog lists rules the README does not: %s" (String.concat ", " unknown)
    )

[<Fact>]
let ``categories partition the rules`` () =
    let counted =
        RuleCatalog.all
        |> List.sumBy (fun c -> (RuleCatalog.codesIn (Set.singleton c)).Count)

    Assert.Equal(RuleCatalog.known.Count, counted)

[<Fact>]
let ``the substantive set is correctness and performance`` () =
    let substantive = RuleCatalog.codesIn RuleCatalog.substantive
    Assert.Contains("FR0075", substantive) // a disposable that never gets disposed
    Assert.Contains("FR0038", substantive) // a needless allocation
    Assert.DoesNotContain("FR0083", substantive) // an empty attribute argument list
    Assert.DoesNotContain("FR0099", substantive) // a line-ending semicolon

[<Fact>]
let ``category names round-trip`` () =
    for category in RuleCatalog.all do
        Assert.Equal(Some category, RuleCatalog.parse (RuleCatalog.name category))

[<Fact>]
let ``an unknown category does not parse`` () =
    Assert.Equal(None, RuleCatalog.parse "urgent")

[<Fact>]
let ``the README's kind summary matches the rules it lists`` () =
    // the summary table states a count per kind; adding a rule moved the real
    // count and left the summary behind, which is exactly the drift this
    // catches
    let readme =
        System.IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "README.md")
        |> System.IO.File.ReadAllText

    let actual =
        Regex.Matches(
            readme,
            @"^\| FR\d{4} \|.*\| (correctness|performance|idiom|cosmetic) \|$",
            RegexOptions.Multiline
        )
        |> Seq.countBy (fun m -> m.Groups.[1].Value)
        |> Map.ofSeq

    let claimed =
        Regex.Matches(
            readme,
            @"^\| `(correctness|performance|idiom|cosmetic)` \|.*\| (\d+) \|$",
            RegexOptions.Multiline
        )
        |> Seq.map (fun m -> m.Groups.[1].Value, int m.Groups.[2].Value)
        |> Map.ofSeq

    Assert.NotEmpty claimed

    for KeyValue(kind, stated) in claimed do
        let counted = actual.TryFind kind |> Option.defaultValue 0
        Assert.True((stated = counted), $"README says %d{stated} %s{kind} rules; it lists %d{counted}")
