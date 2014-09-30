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
  | Exponent -> ( ** )
  | Multiply -> (*)
  | Modulus -> (%)
  | Divide -> (/)
  | Add -> (+)
  | Subtract -> (-)

let operatorSymbols =
  [ "^", Exponent
    "*", Multiply
    "%", Modulus
    "/", Divide
    "+", Add
    "-", Subtract ]

let symbolToOperator = operatorSymbols |> Map.ofList
let operatorToSymbol = operatorSymbols |> List.map (fun (x, y) -> (y, x)) |> Map.ofList

let getOperator str =
  match symbolToOperator |> Map.tryFind str with
  | Some op -> op
  | _ -> failwith <| sprintf "Expected operator, found '%s'" str

let getSymbol op =
  match operatorToSymbol |> Map.tryFind op with
  | Some symbol -> symbol
  | _ -> failwith <| sprintf "Could not find a symbol for operator '%A'" op

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
  | Value _ -> Expression (expr, op, right)
  | Expression (left, op2, right2) ->
    if (getPrecedence op <= getPrecedence op2) then
      Expression(expr, op, right)
    else
      Expression(left, op2, addExpr op right right2)

let op = @"[\/\+\-\*\^%]"
let decimal = @"-?(\.\d+|\d+\.?\d*)"
let scientific = sprintf @"%s[eE]-?\d+" decimal
let tokenPattern = sprintf @"((?<!%s)-|%s|%s|%s)" op scientific decimal op
let tokenRegex = Regex(tokenPattern, RegexOptions.Compiled)

let checkSyntax str =
  tokenRegex.Replace(str, "") |> Seq.filter ((<>)' ') |> Seq.isEmpty

let tokenize str = seq { for x in tokenRegex.Matches(str) do yield x.Value }

let parseExpr str =
  let tokens = tokenize str
  let initial = tokens |> Seq.head |> double |> Value
  tokens
  |> Seq.skip 1
  |> Seq.pairwise
  |> Seq.mapi (fun i x -> i % 2 = 0, x)
  |> Seq.filter fst
  |> Seq.map (fun (_, (op, value)) -> getOperator op, value |> double |> Value)
  |> Seq.fold (fun exprTree (op, value) -> addExpr op value exprTree) initial

let showValue n = System.String.Format("{0:0.###############}", [|n|])

let rec showExpr expr =
  match expr with
  | Value n -> showValue n
  | Expression (left, op, right) ->
    sprintf "(%s%s%s)" (showExpr left) (getSymbol op) (showExpr right)

let (<||>) f g x = f x || g x

[<EntryPoint>]
let main args =
  try
    let str = args.[0]
    if not <| checkSyntax str then
      failwith <| sprintf "Could not parse '%s'" str

    let expr = parseExpr str
    if args |> Seq.exists ((=) "-v" <||> (=) "--verbose") then
      printfn "Tokens: %A" <| (tokenize str |> List.ofSeq)
      printfn "Expression: %s" <| showExpr expr
    printfn "%s" <| (expr |> evalExpr |> showValue)
    0
  with
    | ex ->
      printfn "Error: %s" ex.Message
      -1
