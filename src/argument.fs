namespace Calculator

type ConsoleFlags =
  | Flag of string
  | ParamFlag of string * string

type ArgumentsCollection = {
  rawInputs : string list
  flagsMap : Map<string, ConsoleFlags>
}

module Arguments =
  open System.Text.RegularExpressions

  let private pattern = @"--(?<flag>\w+)(:(?<param>.*))?"
  let private regex = Regex(pattern, RegexOptions.Compiled)

  let parse commandLineArgs =
    let rec matchArguments inputs args arguments =
      match arguments with
      | (next :: rest) ->
        let matchArg = regex.Match(next)
        if matchArg <> null && matchArg.Success then
          let flag = matchArg.Groups.["flag"].Value.ToLower()
          let param = matchArg.Groups.["param"].Value
          if param <> "" then
            matchArguments inputs ((flag, ParamFlag (flag, param)) :: args) rest
          else
            matchArguments inputs ((flag, Flag (flag)) :: args) rest
        else
          matchArguments (next :: inputs) args rest
      | _ -> (inputs, args)

    let (rawInputs, parsedArgs) = matchArguments [] [] (commandLineArgs |> List.ofArray)
    let argumentsCollection = {
      rawInputs = rawInputs |> List.rev
      flagsMap = parsedArgs |> Map.ofList
    }
    argumentsCollection

  let hasFlag flag parsedArgs = parsedArgs.flagsMap |> Map.containsKey flag

  let tryFindFlag flag parsedArgs = parsedArgs.flagsMap |> Map.tryFind flag

  let tryFindParameter flag parsedArgs =
    match parsedArgs |> tryFindFlag flag with
    | Some (ParamFlag (_, param)) -> Some param
    | _ -> None
