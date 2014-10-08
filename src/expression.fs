module FsCalculator.Expression

open FsCalculator.Utility
open FsCalculator.Operators
open FsCalculator.Sign
open FsCalculator.Syntax

type ExpressionTree =
  | Group of Sign * ExpressionTree
  | Expression of ExpressionTree * Operator * ExpressionTree
  | Value of double

let rec show = function
  | ExpressionTree.Group (sign, expr) -> sprintf "%s%s" (sign |> Sign.show) (expr |> show)
  | ExpressionTree.Value value -> sprintf "%s" (showValue value)
  | ExpressionTree.Expression (lhs, op, rhs) ->
    sprintf "(%s%s%s)"
      (lhs |> show)
      (getSymbol op)
      (rhs |> show)

let rec private build op rhs = function
  | Expression (lhs2, op2, rhs2) as lhs ->
    if getPrecedence op <= getPrecedence op2 then
      Expression (lhs, op, rhs)
    else
      Expression (lhs2, op2, rhs2 |> build op rhs)
  | lhs -> Expression (lhs, op, rhs)

let rec parse syntax =
  let rec (|SignedGroup|_|) elems =
    let (sign, rest) =
      match elems with
      | (Syntax.Negate :: xs) -> (Negative, xs)
      | xs -> (Positive, xs)
    match rest with
    | (Syntax.Value value :: xs) -> Some (ExpressionTree.Group (sign, ExpressionTree.Value value), xs)
    | (Syntax.Group _ as group :: xs) -> Some (ExpressionTree.Group (sign, parse group), xs)
    | _ -> None
  let rec parse expr remaining =
    match remaining with
    | (Syntax.Function op :: rest) ->
      match rest with
      | SignedGroup (rhs, rest2) -> parse (expr |> build op rhs) rest2
      | (other :: _) -> failwith <| sprintf "Expecting a value or expression, found '%s'" (other |> Syntax.show)
      | _ -> failwith <| sprintf "Unexpected end of expression: '%s'" (expr |> show)
    | (other :: _) -> failwith <| sprintf "Expecting a function, found '%s'" (other |> Syntax.show)
    | _ -> expr

  match syntax with
  | Syntax.Value value -> ExpressionTree.Value value
  | Syntax.Group elems ->
    match elems with
    | SignedGroup (lhs, rest) -> parse lhs rest
    | (other :: _) -> failwith <| sprintf "Expecting a value or expression, found '%s'" (other |> Syntax.show)
    | _ -> failwith "Expression cannot be empty"
  | other -> failwith <| sprintf "Expecting a value or expression, found '%s'" (other |> Syntax.show)

let rec evaluate = function
  | ExpressionTree.Value n -> n
  | ExpressionTree.Group (Negative, expr) -> -(evaluate expr)
  | ExpressionTree.Group (Positive, expr) -> evaluate expr
  | ExpressionTree.Expression (lhs, op, rhs) ->
    (getEvaluator op) (evaluate lhs) (evaluate rhs)
