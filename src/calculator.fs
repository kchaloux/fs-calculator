open FsCalculator.Utility
open FsCalculator.Syntax
open FsCalculator.Expression

[<EntryPoint>]
let main args =
  try
    let syntax = args.[0] |> FsCalculator.Syntax.lex
    let expression = syntax |> FsCalculator.Expression.parse
    if args |> Seq.exists ((=) "--debug") then
      printfn "Syntax: %s" (syntax |> FsCalculator.Syntax.show)
      printfn "    %A" syntax
      printfn "Expression: %s" (expression |> FsCalculator.Expression.show)
      printfn "    %A" expression
    let result = expression |> FsCalculator.Expression.evaluate
    printfn "%s" (showValue result)
    0
  with
    ex ->
      printfn "Error: %s" ex.Message
      -1