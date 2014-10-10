namespace Calculator.Infix
open Calculator

module Parser =
  open Calculator.Utility
  open Calculator.Tokenizer

  let private operatorPattern =
    sprintf @"[%s]" (Operator.symbols |> List.map ((+) "\\") |> List.reduce (+))
  let private decimalPattern = @"(\.\d+|\d+\.?\d*)"
  let private scientificPattern = sprintf @"%s[eE]-?\d+" decimalPattern
  let private negativePattern = sprintf @"-(?=%s|\()" decimalPattern
  let private tokenPattern =
    sprintf @"(%s|%s|%s|%s|\(|\))" negativePattern scientificPattern decimalPattern operatorPattern

  let rec private buildExpression op rhs lhs =
    match rhs with
    | Expression (lhs2, op2, rhs2) ->
      if (op2 |> Operator.toPrecedence) <= (op |> Operator.toPrecedence) then
        Expression (lhs |> buildExpression op lhs2, op2, rhs2)
      else
        Expression (lhs, op, rhs)
    | _ -> Expression (lhs, op, rhs)

  let parse str =
    let rec parseGroup tokens (nestLevel: int) =
      let (sign, rest) =
        match tokens with
        | ("-" :: xs) -> (Negative, xs)
        | xs -> (Positive, xs)
      match rest with
      | ("(" :: xs) ->
          let (expr, rest2, nl2) = parseTokens xs (nestLevel + 1)
          (ExpressionTree.Group (sign, expr), rest2, nl2)
      | (other :: xs) ->
        match other |> Operator.tryGetOperator with
        | Some op -> failwith <| sprintf "Expecting a value or group, found '%s'" (op |> Operator.show)
        | None -> (ExpressionTree.Group (sign, ExpressionTree.Value (double other)), xs, nestLevel)
      | _ -> failwith "Unexpected end of expression"

    and parseTokens tokens (nestLevel: int) =
      let (lhs, rest, nl2) = parseGroup tokens nestLevel
      match rest with
      | (")" :: xs) -> (lhs, xs, nl2 - 1)
      | (token :: rest2) ->
        match token |> Operator.tryGetOperator with
        | Some operator ->
          let (rhs, rest3, nl3) = parseTokens rest2 nl2
          ((lhs |> buildExpression operator rhs), rest3, nl3)
        | _ -> failwith <| sprintf "Expecting an operator, found '%s'" token
      | _ -> (lhs, [], nl2)

    let tokens = tokenize tokenPattern str
    let (expr, _, nestLevel) = parseTokens tokens 0
    if nestLevel < 0 then
      failwith "Mismatched parentheses - expected '('"
    elif nestLevel > 0 then
      failwith "Mismatched parentheses - expected ')'"
    else
      expr
