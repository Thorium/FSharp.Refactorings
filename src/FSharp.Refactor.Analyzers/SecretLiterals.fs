/// FR0127: provider-format API keys and private keys in string literals.
///
/// Not entropy guessing — each pattern is a provider's DOCUMENTED key
/// format, anchored tightly enough that a match in source is a leaked
/// credential until proven otherwise:
///
///     "sk-ant-api03-..."          Anthropic
///     "sk-proj-..." / "sk-..."    OpenAI
///     "AIza..."                   Google API
///     "ghp_..." / "github_pat_"   GitHub
///     "AKIA..."                   AWS access key id
///     "xoxb-..."                  Slack
///     "-----BEGIN PRIVATE KEY"    PEM material
///
/// Notes only: the remedy (rotate the key, move to configuration or a
/// secret store) happens outside this file. Matching is per string
/// literal, all string kinds.
module FSharp.Refactor.SecretLiterals

open System.Text.RegularExpressions
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type Suggestion = { Range: range; Provider: string }

let private patterns =
    [ "Anthropic", Regex(@"\bsk-ant-[A-Za-z0-9_-]{12,}", RegexOptions.Compiled)
      "OpenAI", Regex(@"\bsk-(proj-)?[A-Za-z0-9]{32,}", RegexOptions.Compiled)
      "Google", Regex(@"\bAIza[0-9A-Za-z_-]{35}", RegexOptions.Compiled)
      "GitHub", Regex(@"\bgh[pousr]_[A-Za-z0-9]{36,}|github_pat_[A-Za-z0-9_]{22,}", RegexOptions.Compiled)
      "AWS", Regex(@"\bAKIA[0-9A-Z]{16}\b", RegexOptions.Compiled)
      "Slack", Regex(@"\bxox[baprs]-[A-Za-z0-9-]{10,}", RegexOptions.Compiled)
      "PEM private key", Regex(@"-----BEGIN [A-Z ]*PRIVATE KEY-----", RegexOptions.Compiled) ]

let find (parseTree: ParsedInput) : Suggestion list =
    let index = AstIndex.ofTree parseTree

    [ for _, e in index.Exprs do
          match e with
          | SynExpr.Const(SynConst.String(text, _, _), r) ->
              match patterns |> List.tryFind (fun (_, rx) -> rx.IsMatch text) with
              | Some(provider, _) -> { Range = r; Provider = provider }
              | None -> ()
          | _ -> () ]
