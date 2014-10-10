namespace Calculator.Rpn
open Calculator

module Parser =
  open System.Text.RegularExpressions
  open Calculator.Utility

  let private operatorPattern =
    sprintf @"[%s]" (Operator.symbols |> List.map ((+) "\\") |> List.reduce (+))
  let private decimalPattern = @"-?(\.\d+|\d+\.?\d*)"
  let private scientificPattern = sprintf @"-?%s[eE]-?\d+" decimalPattern
  let private tokenPattern =
    sprintf @"(%s|%s|%s)" scientificPattern decimalPattern operatorPattern
  let private tokenRegex = Regex(tokenPattern, RegexOptions.Compiled)

  let private tokenize str = seq { for x in tokenRegex.Matches(str) do yield x.Value } |> List.ofSeq

  let parse str =
    let rec buildExpression tokens =
      match tokens with
      | (token :: rest) ->
        match token |> Operator.tryGetOperator with
        | Some operator ->
          let (rhs, rest2) = buildExpression rest
          let (lhs, rest3) = buildExpression rest2
          (ExpressionTree.Expression (lhs, operator, rhs), rest3)
        | _ -> (ExpressionTree.Value (double token), rest)
      | _ -> failwith <| sprintf "Unexpected end of expression - missing values"

    let (expr, remaining) = str |> tokenize |> List.rev |> buildExpression
    if remaining |> Seq.length <> 0 then
      let tokenStr = remaining |> List.reduce (fun acc x -> acc + " " + x)
      failwith <| sprintf "Expected end of expression before '%s'" tokenStr
    else
      expr
