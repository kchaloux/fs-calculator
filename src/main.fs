open Calculator
open Utility

[<EntryPoint>]
let main args =
  try
    printfn "arguments: %A" args
    let parsedArgs = args |> Arguments.parse
    let input =
      match parsedArgs.rawInputs |> Seq.tryPick Some with
      | Some x -> x
      | None _ -> failwith "Could not find an expression to calculate"

    let (parseExpression, showExpression) =
      match parsedArgs |> Arguments.tryFindParameter "read" with
      | Some param ->
        match param.ToLower() with
        | "rpn" -> (Rpn.Parser.parse, Rpn.Parser.show)
        | "infix" -> (Infix.Parser.parse, Infix.Parser.show)
        | other -> failwith <| sprintf "Unrecognized argument for --read flag: '%s'" other
      | _ -> (Infix.Parser.parse, Infix.Parser.show)

    let expression = input |> parseExpression

    if parsedArgs |> Arguments.hasFlag "debug" then
      printfn "Expression: %s" (expression |> Expression.show)
      printfn "    %A" expression

    let output =
      match parsedArgs |> Arguments.tryFindFlag "print" with
      | Some argument ->
          match argument with
          | ParamFlag (_, param) ->
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