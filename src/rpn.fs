namespace Calculator.Rpn
open Calculator

module Parser =
  open Calculator.Utility
  open Calculator.Tokenizer

  let private operatorPattern =
    sprintf @"[%s]" (Operator.symbols |> List.map ((+) "\\") |> List.reduce (+))
  let private decimalPattern = @"-?(\.\d+|\d+\.?\d*)"
  let private scientificPattern = sprintf @"-?%s[eE]-?\d+" decimalPattern
  let private tokenPattern =
    sprintf @"(%s|%s|%s)" scientificPattern decimalPattern operatorPattern

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

    let (expr, remaining) = str |> tokenize tokenPattern |> List.rev |> buildExpression
    if remaining |> Seq.length <> 0 then
      let tokenStr = remaining |> List.reduce (fun acc x -> acc + " " + x)
      failwith <| sprintf "Expected end of expression before '%s'" tokenStr
    else
      expr
