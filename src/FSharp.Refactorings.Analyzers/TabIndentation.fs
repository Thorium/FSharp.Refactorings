/// Refactoring (paste repair): F# forbids TAB characters as whitespace
/// (FS1161), so code pasted from tab-indented sources does not even parse.
/// The fix expands every leading TAB to four spaces, line by line, which
/// usually restores the intended offside structure in one step.
///
/// Works from the SOURCE TEXT alone — the file may not parse, which is
/// the point (like FR0077, this rule repairs broken code).
///
/// Safety rules:
///   - only LEADING whitespace is touched; a tab after code could be
///     inside a string literal
///   - a file containing triple-quoted (""") or verbatim (@") strings is
///     skipped entirely: their literals span lines, so even a leading tab
///     can be string CONTENT there
module FSharp.Refactorings.TabIndentation

open FSharp.Compiler.Text

type Suggestion =
    {
        /// The first offending line's leading whitespace (message anchor).
        Range: range
        /// One edit per tab-indented line: (range, original, replacement).
        Edits: (range * string * string) list
    }

/// Expand tabs in a leading-whitespace segment: each tab becomes four
/// spaces, existing spaces pass through.
let private expand (leading: string) = leading.Replace("\t", "    ")

let find (fileName: string) (source: ISourceText) : Suggestion list =
    let lineCount = source.GetLineCount()

    // multiline string literals could own a leading tab as content
    let mutable hasMultilineStrings = false

    for i in 0 .. lineCount - 1 do
        let line = source.GetLineString i

        if line.Contains "\"\"\"" || line.Contains "@\"" then
            hasMultilineStrings <- true

    if hasMultilineStrings then
        []
    else
        let edits =
            [ for i in 0 .. lineCount - 1 do
                  let line = source.GetLineString i
                  let leadingLen = line.Length - line.TrimStart().Length
                  let leading = line.Substring(0, leadingLen)

                  if leading.Contains '\t' then
                      let range =
                          Range.mkRange fileName (Position.mkPos (i + 1) 0) (Position.mkPos (i + 1) leadingLen)

                      range, leading, expand leading ]

        match edits with
        | [] -> []
        | (firstRange, _, _) :: _ -> [ { Range = firstRange; Edits = edits } ]
