open System.Text.RegularExpressions

type Operator =
  | Exponent
  | Multiply
  | Modulus
  | Divide
  | Add
  | Subtract

type ExpressionTree =
  | Expression of
      left: ExpressionTree *
      operator: Operator *
      right: ExpressionTree
  | Value of value: double

let getOpPrecedence = function
  | Exponent -> 5
  | Multiply -> 3
  | Modulus -> 4
  | Divide -> 4
  | Add -> 2
  | Subtract -> 1

let evalOp = function
  | Exponent -> (fun x y -> x ** y)
  | Multiply -> (*)
  | Modulus -> (%)
  | Divide -> (/)
  | Add -> (+)
  | Subtract -> (-)

let getOp str =
  match str with
  | "^" -> Exponent
  | "*" -> Multiply
  | "%" -> Modulus
  | "/" -> Divide
  | "+" -> Add
  | "-" -> Subtract
  | other -> failwith <| "Expected operator, found '" + other + "'"

let rec evalExpr = function
  | Value x -> x
  | Expression (x, op, y) -> (evalOp op) (evalExpr x) (evalExpr y)

let rec addExpr op right expr =
  match expr with
  | Value left -> Expression (expr, op, right)
  | Expression (left, op2, right2) ->
    let prec1 = getOpPrecedence op
    let prec2 = getOpPrecedence op2
    if (prec1 <= prec2) then
      Expression(expr, op, right)
    else
      Expression(left, op2, addExpr op right right2)

let op = @"[\/\+\-\*\^%]"
let number = @"-?(\.\d+|\d+\.?\d*)"
let tokenPattern = @"((?<!" + op + ")-" + "|" + number + "|" + op + ")"
let tokenRegex = Regex(tokenPattern, RegexOptions.Compiled)

let parseExpr str =
  let tokens = seq { for x in tokenRegex.Matches(str) do yield x.Value }
  let initialValue = tokens |> Seq.head |> double |> Value
  tokens
  |> Seq.skip 1
  |> Seq.pairwise
  |> Seq.mapi (fun i x -> i % 2 = 0, x)
  |> Seq.filter fst
  |> Seq.map (fun (i, (opStr, valueStr)) -> getOp opStr, valueStr |> double |> Value)
  |> Seq.fold (fun exprTree (op, value) -> addExpr op value exprTree) initialValue
  
[<EntryPoint>]
let main args =
  let expr = parseExpr args.[0]
  if ((args |> Array.length) > 1) && args.[1] = "-v" then
    printfn "%A" expr

  printfn "%f" <| evalExpr expr
  0
