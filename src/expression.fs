namespace Calculator

type ExpressionTree =
  | Group of ExpressionSign * ExpressionTree
  | Expression of ExpressionTree * MathOperator * ExpressionTree
  | Value of double

module Expression =
  open Utility

  let rec show = function
    | Group (sign, expr) -> sprintf "%s%s" (sign |> Sign.show) (expr |> show)
    | Value value -> sprintf "%s" (showValue value)
    | Expression (lhs, op, rhs) ->
      sprintf "(%s%s%s)"
        (lhs |> show)
        (op |> Operator.show)
        (rhs |> show)

  let rec evaluate = function
    | Value n -> n
    | Group (Negative, expr) -> -(evaluate expr)
    | Group (Positive, expr) -> evaluate expr
    | Expression (lhs, op, rhs) ->
      (op |> Operator.toFunction) (evaluate lhs) (evaluate rhs)
