module FSharp.Refactor.Tests.ConfigurationTests

open System
open System.IO
open Xunit
open FSharp.Refactor

[<Fact>]
let ``rules default to enabled`` () =
    let rules = Configuration.parse "{}"
    Assert.True(Configuration.isEnabledIn rules "FR0001" "MatchToIf")

[<Fact>]
let ``rule disabled by code`` () =
    let rules = Configuration.parse """{ "rules": { "FR0001": false } }"""
    Assert.False(Configuration.isEnabledIn rules "FR0001" "MatchToIf")
    Assert.True(Configuration.isEnabledIn rules "FR0002" "OptionModule")

[<Fact>]
let ``rule disabled by analyzer name case-insensitively`` () =
    let rules = Configuration.parse """{ "rules": { "conversionMove": false } }"""
    Assert.False(Configuration.isEnabledIn rules "FR0004" "ConversionMove")

[<Fact>]
let ``fsharplint-style enabled object is understood`` () =
    let rules =
        Configuration.parse """{ "rules": { "FR0005": { "enabled": false }, "FR0006": { "enabled": true } } }"""

    Assert.False(Configuration.isEnabledIn rules "FR0005" "CeStrip")
    Assert.True(Configuration.isEnabledIn rules "FR0006" "ActivePattern")

[<Fact>]
let ``rule keys may sit at the root without a rules wrapper`` () =
    let rules = Configuration.parse """{ "FR0007": false }"""
    Assert.False(Configuration.isEnabledIn rules "FR0007" "MutableRemoval")

[<Fact>]
let ``explicit code entry wins over a name entry`` () =
    let rules =
        Configuration.parse """{ "rules": { "FR0001": true, "matchToIf": false } }"""

    Assert.True(Configuration.isEnabledIn rules "FR0001" "MatchToIf")

[<Fact>]
let ``comments and trailing commas are tolerated`` () =
    let rules =
        Configuration.parse
            """{
  // disable the composition hint
  "rules": { "FR0003": false, }
}"""

    Assert.False(Configuration.isEnabledIn rules "FR0003" "Composition")

[<Fact>]
let ``malformed json fails open`` () =
    let rules = Configuration.parse "{ not json at all"
    Assert.True(Configuration.isEnabledIn rules "FR0001" "MatchToIf")

[<Fact>]
let ``unknown keys and non-boolean values are ignored`` () =
    let rules =
        Configuration.parse """{ "ignoreFiles": ["x"], "rules": { "FR0002": "nope", "FR0003": false } }"""

    Assert.True(Configuration.isEnabledIn rules "FR0002" "OptionModule")
    Assert.False(Configuration.isEnabledIn rules "FR0003" "Composition")

[<Fact>]
let ``config file is discovered upward from the analyzed file`` () =
    let root =
        Path.Combine(Path.GetTempPath(), "fsref-cfg-" + Guid.NewGuid().ToString "N")

    let nested = Path.Combine(root, "src", "deep")
    Directory.CreateDirectory nested |> ignore

    try
        File.WriteAllText(Path.Combine(root, Configuration.ConfigFileName), """{ "rules": { "FR0001": false } }""")

        let analyzed = Path.Combine(nested, "Code.fs")
        Assert.False(Configuration.isRuleEnabled analyzed "FR0001" "MatchToIf")
        Assert.True(Configuration.isRuleEnabled analyzed "FR0002" "OptionModule")
    finally
        Directory.Delete(root, true)

[<Fact>]
let ``nearest config wins`` () =
    let root =
        Path.Combine(Path.GetTempPath(), "fsref-cfg-" + Guid.NewGuid().ToString "N")

    let nested = Path.Combine(root, "sub")
    Directory.CreateDirectory nested |> ignore

    try
        File.WriteAllText(Path.Combine(root, Configuration.ConfigFileName), """{ "rules": { "FR0001": false } }""")
        File.WriteAllText(Path.Combine(nested, Configuration.ConfigFileName), """{ "rules": { "FR0002": false } }""")

        let analyzed = Path.Combine(nested, "Code.fs")
        // the nested config is the effective one; the outer one is not merged
        Assert.False(Configuration.isRuleEnabled analyzed "FR0002" "OptionModule")
        Assert.True(Configuration.isRuleEnabled analyzed "FR0001" "MatchToIf")
    finally
        Directory.Delete(root, true)

[<Fact>]
let ``no config file means everything enabled`` () =
    let root =
        Path.Combine(Path.GetTempPath(), "fsref-cfg-" + Guid.NewGuid().ToString "N")

    Directory.CreateDirectory root |> ignore

    try
        let analyzed = Path.Combine(root, "Code.fs")
        Assert.True(Configuration.isRuleEnabled analyzed "FR0001" "MatchToIf")
    finally
        Directory.Delete(root, true)

[<Fact>]
let ``FR0099 is off by default`` () =
    // it lexes every file containing a line-ending semicolon and rarely
    // finds anything — cost out of proportion to a cosmetic default
    let rules = Configuration.parse "{}"
    Assert.False(Configuration.isEnabledIn rules "FR0099" "TrailingSemicolon")

[<Fact>]
let ``the configuration can turn FR0099 back on`` () =
    let rules = Configuration.parse """{ "rules": { "FR0099": true } }"""
    Assert.True(Configuration.isEnabledIn rules "FR0099" "TrailingSemicolon")

[<Fact>]
let ``an explicit --codes ask outranks the default-off status`` () =
    Environment.SetEnvironmentVariable("FSREF_FORCE_CODES", "FR0002,FR0099")

    try
        Assert.True(Configuration.isRuleEnabled "Test.fs" "FR0099" "TrailingSemicolon")
    finally
        Environment.SetEnvironmentVariable("FSREF_FORCE_CODES", null)

    Assert.False(Configuration.isRuleEnabled "Test.fs" "FR0099" "TrailingSemicolon")
