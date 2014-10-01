open System.Text.RegularExpressions

type Operator = | Exponent | Multiply | Modulus | Divide | Add | Subtract

let getPrecedence = function
  | Exponent -> 3
  | Multiply -> 2
  | Modulus -> 2
  | Divide -> 2
  | Add -> 1
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
  | Expression of ExpressionTree * Operator * ExpressionTree
  | Value of double
  | Empty

let showValue n = System.String.Format("{0:0.###############}", [|n|])

let showExpr expr f =
  match expr with
  | Empty -> "_"
  | Value n -> showValue n
  | Expression (lhs, op, rhs) -> f lhs op rhs

let rec showInfixExpr expr = showExpr expr (fun lhs op rhs ->
  sprintf "(%s %s %s)" (showInfixExpr lhs) (getSymbol op) (showInfixExpr rhs))

let rec showPostfixExpr expr = showExpr expr (fun lhs op rhs ->
  sprintf "%s %s %s" (showPostfixExpr lhs) (showPostfixExpr rhs) (getSymbol op))

let rec showPrefixExpr expr = showExpr expr (fun lhs op rhs ->
  sprintf "%s %s %s" (getSymbol op) (showPrefixExpr lhs) (showPrefixExpr rhs))

let rec evalExpr expr =
  match expr with
  | Value x -> x
  | Expression (x, op, y) -> (getEvaluator op) (evalExpr x) (evalExpr y)
  | Empty -> failwith <| sprintf "Error: Expression is incomplete"

let rec addExpr value expr =
  match expr with
  | Empty -> value
  | Expression (Empty, op, Empty) -> Expression (Empty, op, value)
  | Expression (Empty, op, rhs) -> Expression (value, op, rhs)
  | Expression (lhs, op, Empty) -> Expression (lhs, op, value)
  | Expression (lhs, op, rhs) -> Expression (lhs, op, addExpr value rhs)
  | _ ->
    sprintf "Error: Cannot evaluate expression: %s %s" (showInfixExpr expr) (showInfixExpr value)
    |> failwith

let rec addOperator op expr =
  match expr with
  | Empty -> Expression (Empty, op, Empty)
  | Value _ -> Expression (expr, op, Empty)
  | Expression (lhs, op2, rhs) ->
    if (getPrecedence op <= getPrecedence op2) then
      Expression (expr, op, Empty)
    else
      Expression (lhs, op2, addOperator op rhs)

let addToken token expr =
  match symbolToOperator |> Map.tryFind token with
  | Some op -> addOperator op expr
  | None -> addExpr (Value (double token)) expr

let op = @"[\/\+\-\*\^%]"
let decimal = @"-?(\.\d+|\d+\.?\d*)"
let scientific = sprintf @"%s[eE]-?\d+" decimal
let tokenPattern = sprintf @"((?<!%s)-|%s|%s|%s|\(|\))" op scientific decimal op
let tokenRegex = Regex(tokenPattern, RegexOptions.Compiled)

let checkSyntax str =
  tokenRegex.Replace(str, "") |> Seq.filter ((<>)' ') |> Seq.isEmpty

let tokenize str = seq { for x in tokenRegex.Matches(str) do yield x.Value }

let parseExpr str =
  let rec parse expr current tokens =
    match tokens with
    | ("("::rest) -> parse (addExpr current expr) Empty rest
    | (")"::rest) -> parse Empty (addExpr current expr) rest
    | (token::rest) -> parse expr (addToken token current) rest
    | _ -> current
  let tokens = tokenize str
  let initial =
    match tokens |> Seq.head with
    | "(" -> Empty
    | x -> Value (double x)
  parse Empty initial (tokens |> Seq.skip 1 |> List.ofSeq)

let flip f x y = f y x

[<EntryPoint>]
let main args =
  try
    let str = args.[0]
    if not <| checkSyntax str then
      failwith <| sprintf "Could not parse '%s'" str

    let expr = parseExpr str
    if args |> Seq.exists ((=) "--print") then
      let showFunctionNames =
        [ "--postfix", showPostfixExpr
          "--infix", showInfixExpr
          "--prefix", showPrefixExpr
        ] |> Map.ofList
      let showFunction =
        match args |> Seq.tryPick (flip Map.tryFind showFunctionNames) with
        | Some f -> f
        | _      -> showInfixExpr
      printfn "%s" <| showFunction expr
    else
      if (args |> Seq.exists ((=) "--debug")) then
        printfn "Tokens: %A" <| (tokenize str |> List.ofSeq)
        printfn "Expression: %s" (showInfixExpr expr)
      printfn "%s" <| (expr |> evalExpr |> showValue)
    0
  with
    | ex ->
      printfn "Error: %s" ex.Message
      -1
