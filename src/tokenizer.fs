namespace Calculator

module Tokenizer =
  open System.Text.RegularExpressions

  let private prependBad (token: string) bad =
    let trimmed = token.Trim()
    if trimmed = "" then
      bad
    else
      (trimmed :: bad)

  let tokenize pattern str =
    let regex = Regex(pattern)
    let rec collectTokens good bad rest =
      let tokenMatch = regex.Match(rest)
      if (not tokenMatch.Success) || rest = "" then
        (good, (prependBad rest bad))
      else
        let token = tokenMatch.Value
        let index = tokenMatch.Index
        let length = tokenMatch.Length
        let suffix = rest.[(index + length)  .. (rest.Length - 1)]
        if index <> 0 then
          let prefix = rest.[0 .. (index - 1)]
          collectTokens (token :: good) (prependBad prefix bad) suffix
        else
          collectTokens (token :: good) bad suffix

    let (good, bad) = collectTokens [] [] str
    if bad |> Seq.length <> 0 then
      failwith <| sprintf "Unrecognized tokens: %A" (bad |> List.rev)
    else
      good |> List.rev
