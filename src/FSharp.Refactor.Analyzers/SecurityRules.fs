/// Two security notes:
///
/// 1. Weak cryptography (FR0065, CA5350/CA5351): MD5, SHA1, DES,
///    TripleDES and RC2 are broken for security purposes — collisions
///    and key sizes are within practical attack range. SHA-256+ and AES
///    are the drop-in families. (Non-security uses like legacy checksums
///    are a decision — this is a note, not a gate.) Also flags disabling
///    TLS certificate validation, which silently accepts any
///    man-in-the-middle.
///
/// 2. SQL built from strings (FR0066, CA2100): a command text assembled
///    by concatenation, interpolation holes, or sprintf invites
///    injection; parameters (`@name` + Parameters.AddWithValue) keep the
///    data out of the query language.
module FSharp.Refactor.SecurityRules

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Refactor.Text

[<RequireQualifiedAccess>]
type WeakKind =
    | Hash of name: string
    | Cipher of name: string
    | CertificateBypass
    /// Ssl3/Tls/Tls11 protocol constants: broken or deprecated on the wire.
    | Protocol of name: string

type WeakCryptoSuggestion =
    { Range: range
      Kind: WeakKind
      /// The algorithm identifier itself, when a swap fix can target it.
      AlgoRange: range option }

/// A dynamically built string reaching a process-execution sink — the
/// command-injection shape SonarQube's agentic-workflow rules target,
/// which matters doubly when the string carries LLM output.
type ProcessSinkSuggestion = { Range: range; Sink: string }

type SqlStringSuggestion =
    {
        Range: range
        /// "CommandText" or the constructor's type name.
        Sink: string
    }

let private weakHashes = set [ "MD5"; "SHA1" ]

let private weakHashTypes =
    set
        [ "MD5CryptoServiceProvider"
          "SHA1CryptoServiceProvider"
          "SHA1Managed"
          "MD5Cng" ]

let private weakCiphers = set [ "DES"; "TripleDES"; "RC2" ]

let private weakCipherTypes =
    set
        [ "DESCryptoServiceProvider"
          "TripleDESCryptoServiceProvider"
          "RC2CryptoServiceProvider" ]

let private commandTypes =
    set
        [ "SqlCommand"
          "NpgsqlCommand"
          "MySqlCommand"
          "OracleCommand"
          "SqliteCommand"
          "SQLiteCommand" ]

/// A string expression assembled at runtime: interpolation with holes,
/// a `+` chain, or a sprintf/String.Format call.
let private isDynamicString (e: SynExpr) =
    match stripParens e with
    | SynExpr.InterpolatedString(contents = parts) ->
        parts
        |> List.exists (fun p ->
            match p with
            | SynInterpolatedStringPart.FillExpr _ -> true
            | SynInterpolatedStringPart.String _ -> false)
    // infix + parses its operator as a one-segment LongIdent, not Ident
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = IdentName "op_Addition")) -> true
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = [ op ])))) when
        op.idText = "op_Addition"
        ->
        true
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = SingleIdent f)) when f.idText = "sprintf" -> true
    | SynExpr.App(funcExpr = SingleIdent f) when f.idText = "sprintf" -> true
    | SynExpr.App(funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))) when
        ids.Length >= 2
        && (List.last ids).idText = "Format"
        && ids.[ids.Length - 2].idText = "String"
        ->
        true
    | _ -> false

/// Find weak cryptography, string-built SQL, and string-built process
/// execution.
let find
    (parseTree: ParsedInput)
    (source: ISourceText)
    : WeakCryptoSuggestion list * SqlStringSuggestion list * ProcessSinkSuggestion list =
    ignore source
    let index = AstIndex.ofTree parseTree
    let crypto = ResizeArray<WeakCryptoSuggestion>()
    let sql = ResizeArray<SqlStringSuggestion>()
    let processSinks = ResizeArray<ProcessSinkSuggestion>()

    for _, e in index.Exprs do
        match e with
        // MD5.Create() / SHA1.Create() / DES.Create() ...
        | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))) when
            ids.Length >= 2 && (List.last ids).idText = "Create"
            ->
            match List.rev ids with
            | _create :: ownerId :: _ ->
                let owner = ownerId.idText

                if weakHashes.Contains owner then
                    crypto.Add
                        { Range = e.Range
                          Kind = WeakKind.Hash owner
                          AlgoRange = Some ownerId.idRange }
                elif weakCiphers.Contains owner then
                    crypto.Add
                        { Range = e.Range
                          Kind = WeakKind.Cipher owner
                          AlgoRange = Some ownerId.idRange }
            | _ -> ()
        // new MD5CryptoServiceProvider() and friends
        | SynExpr.New(targetType = SynType.LongIdent(SynLongIdent(id = ids))) when not ids.IsEmpty ->
            let name = (List.last ids).idText

            if weakHashTypes.Contains name then
                crypto.Add
                    { Range = e.Range
                      Kind = WeakKind.Hash name
                      AlgoRange = None }
            elif weakCipherTypes.Contains name then
                crypto.Add
                    { Range = e.Range
                      Kind = WeakKind.Cipher name
                      AlgoRange = None }
            elif commandTypes.Contains name then
                // new SqlCommand(dynamicSql, ...)
                match e with
                | SynExpr.New(expr = arg) ->
                    let first =
                        match stripParens arg with
                        | SynExpr.Tuple(exprs = head :: _) -> head
                        | single -> single

                    if isDynamicString first then
                        sql.Add { Range = e.Range; Sink = name }
                | _ -> ()
            elif name = "ProcessStartInfo" then
                // new ProcessStartInfo(dynamicFile, dynamicArgs): the
                // command/argument-injection sink
                match e with
                | SynExpr.New(expr = arg) ->
                    let argsOf =
                        match stripParens arg with
                        | SynExpr.Tuple(exprs = es) -> es
                        | single -> [ single ]

                    if argsOf |> List.exists isDynamicString then
                        processSinks.Add
                            { Range = e.Range
                              Sink = "ProcessStartInfo" }
                | _ -> ()
        // SqlCommand(dynamicSql, ...) without `new`
        | SynExpr.App(isInfix = false; funcExpr = SingleIdent ctor; argExpr = arg) when
            commandTypes.Contains ctor.idText
            ->
            let first =
                match stripParens arg with
                | SynExpr.Tuple(exprs = head :: _) -> head
                | single -> single

            if isDynamicString first then
                sql.Add { Range = e.Range; Sink = ctor.idText }
        // cmd.CommandText <- dynamicSql; cert validation bypass
        | SynExpr.LongIdentSet(SynLongIdent(id = ids), rhs, _) when not ids.IsEmpty ->
            match (List.last ids).idText with
            | "CommandText" when isDynamicString rhs ->
                sql.Add
                    { Range = e.Range
                      Sink = "CommandText" }
            | "ServerCertificateValidationCallback"
            | "ServerCertificateCustomValidationCallback" ->
                crypto.Add
                    { Range = e.Range
                      Kind = WeakKind.CertificateBypass
                      AlgoRange = None }
            // psi.Arguments <- dynamic: the argument-injection sink;
            // FileName is any DTO's field and stays out
            | "Arguments" when isDynamicString rhs ->
                processSinks.Add
                    { Range = e.Range
                      Sink = "Arguments" }
            | _ -> ()
        | SynExpr.DotSet(_, SynLongIdent(id = ids), rhs, _) when not ids.IsEmpty ->
            match (List.last ids).idText with
            | "CommandText" when isDynamicString rhs ->
                sql.Add
                    { Range = e.Range
                      Sink = "CommandText" }
            | "ServerCertificateValidationCallback"
            | "ServerCertificateCustomValidationCallback" ->
                crypto.Add
                    { Range = e.Range
                      Kind = WeakKind.CertificateBypass
                      AlgoRange = None }
            | "Arguments" when isDynamicString rhs ->
                processSinks.Add
                    { Range = e.Range
                      Sink = "Arguments" }
            | _ -> ()
        // Process.Start with a dynamically built command — the
        // command-injection sink; distinctive by name, so no typed gate
        | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = pids)); argExpr = parg) when
            pids.Length >= 2
            && (List.last pids).idText = "Start"
            && (pids |> List.item (pids.Length - 2)).idText = "Process"
            ->
            let argsOf =
                match stripParens parg with
                | SynExpr.Tuple(exprs = es) -> es
                | single -> [ single ]

            if argsOf |> List.exists isDynamicString then
                processSinks.Add
                    { Range = e.Range
                      Sink = "Process.Start" }
        // SecurityProtocolType.Ssl3 / SslProtocols.Tls11 and friends:
        // broken or deprecated on the wire. The modern default is to set
        // NOTHING and let the OS negotiate
        | SynExpr.LongIdent(longDotId = SynLongIdent(id = ids)) when ids.Length >= 2 ->
            match (ids |> List.item (ids.Length - 2)).idText, (List.last ids).idText with
            | ("SecurityProtocolType" | "SslProtocols"), ("Ssl2" | "Ssl3" | "Tls" | "Tls11" as proto) ->
                crypto.Add
                    { Range = e.Range
                      Kind = WeakKind.Protocol proto
                      // the constant ident itself: the Tls12 swap's target
                      AlgoRange = Some (List.last ids).idRange }
            | _ -> ()
        | _ -> ()

    List.ofSeq crypto, List.ofSeq sql, List.ofSeq processSinks
