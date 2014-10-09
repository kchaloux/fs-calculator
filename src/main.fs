open Calculator
open Utility

[<EntryPoint>]
let main args =
  try
    let expression = args.[0] |> Infix.Parser.parse
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