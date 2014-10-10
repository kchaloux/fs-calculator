open Calculator
open Utility

[<EntryPoint>]
let main args =
  try
    let input = args.[0]
    let expression = 
      if args |> Seq.exists ((=) "--rpn") then
        input |> Rpn.Parser.parse
      else
        input |> Infix.Parser.parse

    if args |> Seq.exists ((=) "--debug") then
      printfn "Expression: %s" (expression |> Expression.show)
      printfn "    %A" expression

    let result = expression |> Expression.evaluate
    printfn "%s" (showValue result)
    0
  with
    ex ->
      printfn "Error: %s" ex.Message
      -1