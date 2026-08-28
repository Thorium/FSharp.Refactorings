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

type WeakCryptoSuggestion = { Range: range; Kind: WeakKind }

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
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = IdentName "op_Addition")) -> true
    | SynExpr.App(funcExpr = SynExpr.App(funcExpr = SingleIdent f)) when f.idText = "sprintf" -> true
    | SynExpr.App(funcExpr = SingleIdent f) when f.idText = "sprintf" -> true
    | SynExpr.App(funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))) when
        ids.Length >= 2
        && (List.last ids).idText = "Format"
        && ids.[ids.Length - 2].idText = "String"
        ->
        true
    | _ -> false

/// Find weak cryptography and string-built SQL.
let find (parseTree: ParsedInput) (source: ISourceText) : WeakCryptoSuggestion list * SqlStringSuggestion list =
    ignore source
    let index = AstIndex.ofTree parseTree
    let crypto = ResizeArray<WeakCryptoSuggestion>()
    let sql = ResizeArray<SqlStringSuggestion>()

    for _, e in index.Exprs do
        match e with
        // MD5.Create() / SHA1.Create() / DES.Create() ...
        | SynExpr.App(isInfix = false; funcExpr = SynExpr.LongIdent(longDotId = SynLongIdent(id = ids))) when
            ids.Length >= 2 && (List.last ids).idText = "Create"
            ->
            let owner = ids.[ids.Length - 2].idText

            if weakHashes.Contains owner then
                crypto.Add
                    { Range = e.Range
                      Kind = WeakKind.Hash owner }
            elif weakCiphers.Contains owner then
                crypto.Add
                    { Range = e.Range
                      Kind = WeakKind.Cipher owner }
        // new MD5CryptoServiceProvider() and friends
        | SynExpr.New(targetType = SynType.LongIdent(SynLongIdent(id = ids))) when not ids.IsEmpty ->
            let name = (List.last ids).idText

            if weakHashTypes.Contains name then
                crypto.Add
                    { Range = e.Range
                      Kind = WeakKind.Hash name }
            elif weakCipherTypes.Contains name then
                crypto.Add
                    { Range = e.Range
                      Kind = WeakKind.Cipher name }
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
                      Kind = WeakKind.CertificateBypass }
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
                      Kind = WeakKind.CertificateBypass }
            | _ -> ()
        | _ -> ()

    List.ofSeq crypto, List.ofSeq sql
