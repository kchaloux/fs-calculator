namespace Calculator

type ConsoleArgument =
  | Flag of string
  | Option of string * string

module Argument =
  open System.Text.RegularExpressions

  let private pattern = @"--(?<flag>\w+)(:(?<param>.*))?"
  let private regex = Regex(pattern, RegexOptions.Compiled)

  let toFlag = function
    | Flag flag -> flag
    | Option (flag, _) -> flag

  let parse commandLineArgs =
    let matchArgument arg =
      let matchArg = regex.Match(arg)
      if matchArg <> null then
        let flag = matchArg.Groups.["flag"].Value.ToLower()
        let param = matchArg.Groups.["param"].Value
        if param <> "" then
          Some (Option (flag, param))
        else
          Some (Flag flag)
      else
        None

    commandLineArgs
    |> Seq.choose matchArgument
    |> Seq.map (fun x -> (toFlag x, x))
    |> Map.ofSeq

  let exists flag parsedArgs = parsedArgs |> Map.containsKey flag

  let tryFind flag parsedArgs = parsedArgs |> Map.tryFind flag

  let tryFindParameter flag parsedArgs =
    match parsedArgs |> tryFind flag with
    | Some (Option (_, param)) -> Some param
    | _ -> None
