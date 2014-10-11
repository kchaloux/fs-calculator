open Calculator
open Utility

[<EntryPoint>]
let main args =
  try
    let input = args.[0]
    let commandArgs = args |> Seq.skip 1 |> Argument.parse

    let (parseExpression, showExpression) =
      match commandArgs |> Argument.tryFindParameter "read" with
      | Some param ->
        match param.ToLower() with
        | "rpn" -> (Rpn.Parser.parse, Rpn.Parser.show)
        | "infix" -> (Infix.Parser.parse, Infix.Parser.show)
        | other -> failwith <| sprintf "Unrecognized argument for --read flag: '%s'" other
      | _ -> (Infix.Parser.parse, Infix.Parser.show)

    let expression = input |> parseExpression

    if commandArgs |> Argument.exists "debug" then
      printfn "Expression: %s" (expression |> Expression.show)
      printfn "    %A" expression

    let output =
      match commandArgs |> Argument.tryFind "print" with
      | Some argument ->
          match argument with
          | Option (_, param) ->
            match param.ToLower() with
            | "rpn" -> expression |> Rpn.Parser.show
            | "infix" -> expression |> Infix.Parser.show
            | other -> failwith <| sprintf "Unrecognized argument for --print flag: '%s'" other
          | _ -> expression |> showExpression
      | _ -> expression |> Expression.evaluate |> showValue
    printfn "%s" output

    0
  with
    ex ->
      printfn "Error: %s" ex.Message
      -1