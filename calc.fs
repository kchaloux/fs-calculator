open System.Text.RegularExpressions

type Operator = | Exponent | Multiply | Modulus | Divide | Add | Subtract

let getPrecedence = function
  | Exponent -> 5
  | Multiply -> 3
  | Modulus -> 4
  | Divide -> 4
  | Add -> 2
  | Subtract -> 1

let getEvaluator = function
  | Exponent -> (fun x y -> x ** y)
  | Multiply -> (*)
  | Modulus -> (%)
  | Divide -> (/)
  | Add -> (+)
  | Subtract -> (-)

let getOperator str =
  match str with
  | "^" -> Exponent
  | "*" -> Multiply
  | "%" -> Modulus
  | "/" -> Divide
  | "+" -> Add
  | "-" -> Subtract
  | other -> failwith <| "Expected operator, found '" + other + "'"

type ExpressionTree =
  | Expression of
      left: ExpressionTree *
      operator: Operator *
      right: ExpressionTree
  | Value of value: double

let rec evalExpr = function
  | Value x -> x
  | Expression (x, op, y) -> (getEvaluator op) (evalExpr x) (evalExpr y)

let rec addExpr op right expr =
  match expr with
  | Value left -> Expression (expr, op, right)
  | Expression (left, op2, right2) ->
    if (getPrecedence op <= getPrecedence op2) then
      Expression(expr, op, right)
    else
      Expression(left, op2, addExpr op right right2)

let op = @"[\/\+\-\*\^%]"
let number = @"-?(\.\d+|\d+\.?\d*)"
let tokenPattern = @"((?<!" + op + ")-" + "|" + number + "|" + op + ")"
let tokenRegex = Regex(tokenPattern, RegexOptions.Compiled)

let checkSyntax str =
  tokenRegex.Replace(str, "") |> Seq.filter ((<>)' ') |> Seq.isEmpty

let parseExpr str =
  let tokens = seq { for x in tokenRegex.Matches(str) do yield x.Value }
  let initial = tokens |> Seq.head |> double |> Value
  tokens
  |> Seq.skip 1
  |> Seq.pairwise
  |> Seq.mapi (fun i x -> i % 2 = 0, x)
  |> Seq.filter fst
  |> Seq.map (fun (i, (op, value)) -> getOperator op, value |> double |> Value)
  |> Seq.fold (fun exprTree (op, value) -> addExpr op value exprTree) initial

let (<||>) f g x = f x || g x

[<EntryPoint>]
let main args =
  try
    if not <| checkSyntax args.[0] then
      failwith <| "Could not parse '" + args.[0] + "'"

    let expr = parseExpr args.[0]
    if args |> Seq.exists ((=) "-v" <||> (=) "--verbose") then
      printfn "%A" expr
    printfn "%f" <| evalExpr expr
    0
  with
    | ex ->
      printfn "Error: %s" ex.Message
      -1
