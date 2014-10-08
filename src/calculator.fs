open System.Text.RegularExpressions

let flip f x y = f y x

type Operator = Exponent | Multiply | Modulus | Divide | Add | Subtract

type Sign = Positive | Negative

type ExpressionTree =
  | Group of Sign * ExpressionTree
  | Expression of ExpressionTree * Operator * ExpressionTree
  | Value of double

type Syntax =
  | Negate
  | Value of double
  | Function of Operator
  | Group of Syntax list
  | GroupOpen
  | GroupClose

let operatorSymbols =
  [ Exponent, "^"
    Multiply, "*"
    Modulus, "%"
    Divide, "/"
    Add, "+"
    Subtract, "-" ]

let getOperator symbol =
  match operatorSymbols |> Seq.tryFind (fun (_, str) -> str = symbol) with
  | Some (operator, _) -> operator
  | _ -> failwith <| sprintf "Could not find a matching operator for the symbol '%s'" symbol

let getSymbol operator =
  match operatorSymbols |> Seq.tryFind (fun (op, _) -> op = operator) with
  | Some (_, symbol) -> symbol
  | _ -> failwith <| sprintf "Could not find a matching symbol for the operator '%A'" operator

let getPrecedence = function
  | Exponent -> 3
  | Multiply -> 2
  | Modulus -> 2
  | Divide ->  2
  | Add -> 1
  | Subtract -> 1

let getEvaluator = function
  | Exponent -> ( ** )
  | Multiply -> ( * )
  | Modulus -> ( % )
  | Divide -> ( / )
  | Add -> ( + )
  | Subtract -> ( - )

let operatorPattern =
  sprintf @"[%s]"
    (operatorSymbols
    |> List.map (fun (_, symbol) -> "\\" + symbol)
    |> List.reduce (+))
let decimalPattern = @"(\.\d+|\d+\.?\d*)"
let scientificPattern = sprintf @"%s[eE]-?\d+" decimalPattern
let negativePattern = sprintf @"-(?=%s|\()" decimalPattern
let tokenPattern =
  sprintf @"(%s|%s|%s|%s|\(|\))" negativePattern scientificPattern decimalPattern operatorPattern
let tokenRegex = Regex(tokenPattern, RegexOptions.Compiled)

let showSign = function
  | Sign.Positive -> ""
  | Sign.Negative -> "-"

let showValue n = System.String.Format("{0:0.###############}", [|n|])

let tokenize str = seq { for x in tokenRegex.Matches(str) do yield x.Value } |> List.ofSeq

let rec showSyntax = function
  | Syntax.Value n -> showValue n
  | Syntax.Negate -> "-"
  | Syntax.GroupOpen -> "("
  | Syntax.GroupClose -> ")"
  | Syntax.Function op -> getSymbol op
  | Syntax.Group elems -> elems |> List.map (showSyntax) |> List.fold (+) "" |> sprintf "(%s)"

let rec showExpression = function
  | ExpressionTree.Group (sign, expr) -> sprintf "%s%s" (showSign sign) (showExpression expr)
  | ExpressionTree.Value value -> sprintf "%s" (showValue value)
  | ExpressionTree.Expression (lhs, op, rhs) ->
    sprintf "(%s%s%s)"
      (showExpression lhs)
      (getSymbol op)
      (showExpression rhs)

let (>~) (elem: Syntax) (group: Syntax) =
  match group with
  | Syntax.Group elems -> Syntax.Group (elem :: elems)
  | _ -> failwith <| sprintf "Cannot add element to group '%A'" group

let checkSyntax token previous =
  match token with
  | "-" ->
    match previous with
    | Some (Syntax.Value _ | Syntax.Group _) -> Syntax.Function Subtract
    | Some (Syntax.Function _) | None -> Syntax.Negate
    | Some other -> failwith <| sprintf "Unexpected '-' after '%s'" (showSyntax other)
  | "(" ->
    match previous with
    | Some (Syntax.Function _ | Syntax.Negate) | None -> Syntax.GroupOpen
    | Some other -> failwith <| sprintf "Cannot begin a group after '%s'" (showSyntax other)
  | ")" ->
    match previous with
    | Some (Syntax.Value _ | Syntax.Group _) -> Syntax.GroupClose
    | None -> failwith <| "Mismatched parentheses - expected '('"
    | Some other -> failwith <| sprintf "Cannot close a group after '%s'" (showSyntax other)
  | other ->
    match operatorSymbols |> Seq.tryFind (fun (_, symbol) -> symbol = other) with
    | Some (operator, _) ->
      match previous with
      | Some (Syntax.Value _ | Syntax.Group _) -> Syntax.Function operator
      | _ -> failwith <| sprintf "Cannot parse expression at '%s'" other
    | _ -> Syntax.Value (double other)

let lexSyntax str =
  let rec lex groups top previous remaining =
    match remaining with
    | (token :: rest) ->
      let syntax = checkSyntax token previous
      match syntax with
      | Syntax.GroupOpen -> lex (top :: groups) (Syntax.Group []) None rest
      | Syntax.GroupClose ->
        match groups with
        | (groupHead :: groupTail) -> lex groupTail (top >~ groupHead) (Some top) rest
        | [] -> failwith "Mismatched parentheses - expected '('"
      | _ -> lex groups (syntax >~ top) (Some syntax) rest
    | _ -> top :: groups
  if tokenRegex.Replace(str, "").Trim() <> "" then
    failwith <| sprintf "Unexpected tokens in input - %s" str
  let tokens = tokenize str
  let groups = lex [] (Syntax.Group []) None tokens
  if groups |> Seq.length = 1 then
    let rec reverseTree = function
    | Syntax.Group elems -> Syntax.Group (elems |> List.rev |> List.map reverseTree)
    | other -> other
    reverseTree groups.[0]
  else
    failwith "Mismatched parentheses - expected ')'"

let rec buildExpression op rhs = function
  | Expression (lhs2, op2, rhs2) as lhs ->
    if getPrecedence op <= getPrecedence op2 then
      Expression (lhs, op, rhs)
    else
      Expression (lhs2, op2, rhs2 |> buildExpression op rhs)
  | lhs -> Expression (lhs, op, rhs)

let rec parseExpression syntax =
  let (|SignedGroup|_|) elems =
    let (sign, rest) =
      match elems with
      | (Syntax.Negate :: xs) -> (Negative, xs)
      | xs -> (Positive, xs)
    match rest with
    | (Syntax.Value value :: xs) -> Some (ExpressionTree.Group (sign, ExpressionTree.Value value), xs)
    | (Syntax.Group _ as group :: xs) -> Some (ExpressionTree.Group (sign, parseExpression group), xs)
    | _ -> None

  let rec parse expr remaining =
    match remaining with
    | (Syntax.Function op :: rest) ->
      match rest with
      | SignedGroup (rhs, rest2) -> parse (expr |> buildExpression op rhs) rest2
      | (other :: _) -> failwith <| sprintf "Expecting a value or expression, found '%s'" (showSyntax other)
      | _ -> failwith <| sprintf "Unexpected end of expression: '%s'" (showExpression expr)
    | (other :: _) -> failwith <| sprintf "Expecting a function, found '%s'" (showSyntax other)
    | _ -> expr

  match syntax with
  | Syntax.Value value -> ExpressionTree.Value value
  | Syntax.Group elems ->
    match elems with
    | SignedGroup (lhs, rest) -> parse lhs rest
    | (other :: _) -> failwith <| sprintf "Expecting a value or expression, found '%s'" (showSyntax other)
    | _ -> failwith "Expression cannot be empty"
  | other -> failwith <| sprintf "Expecting a value or expression, found '%s'" (showSyntax other)

let rec evaluateExpression = function
  | ExpressionTree.Value n -> n
  | ExpressionTree.Group (Negative, expr) -> -(evaluateExpression expr)
  | ExpressionTree.Group (Positive, expr) -> evaluateExpression expr
  | ExpressionTree.Expression (lhs, op, rhs) ->
    (getEvaluator op) (evaluateExpression lhs) (evaluateExpression rhs)

[<EntryPoint>]
let main args =
  try
    let syntax = lexSyntax args.[0]
    let expression = parseExpression syntax
    if args |> Seq.exists ((=) "--debug") then
      printfn "Syntax: %s" (showSyntax syntax)
      printfn "    %A" syntax
      printfn "Expression: %s" (showExpression expression)
      printfn "    %A" expression
    let result = evaluateExpression expression
    printfn "%s" (showValue result)
    0
  with
    ex ->
      printfn "Error: %s" ex.Message
      -1