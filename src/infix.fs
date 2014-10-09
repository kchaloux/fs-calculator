namespace Calculator.Infix
open Calculator

type Syntax =
  | Negate
  | Value of double
  | Function of Calculator.MathOperator
  | Group of Syntax list
  | GroupOpen
  | GroupClose

module Parser =
  open System.Text.RegularExpressions
  open Calculator.Utility

  let private operatorPattern =
    sprintf @"[%s]" (Operator.symbols |> List.map ((+) "\\") |> List.reduce (+))
  let private decimalPattern = @"(\.\d+|\d+\.?\d*)"
  let private scientificPattern = sprintf @"%s[eE]-?\d+" decimalPattern
  let private negativePattern = sprintf @"-(?=%s|\()" decimalPattern
  let private tokenPattern =
    sprintf @"(%s|%s|%s|%s|\(|\))" negativePattern scientificPattern decimalPattern operatorPattern
  let private tokenRegex = Regex(tokenPattern, RegexOptions.Compiled)

  let private tokenize str = seq { for x in tokenRegex.Matches(str) do yield x.Value } |> List.ofSeq

  let rec show = function
    | Value n -> showValue n
    | Negate -> "-"
    | GroupOpen -> "("
    | GroupClose -> ")"
    | Function op -> op |> Operator.show
    | Group elems -> elems |> List.map (show) |> List.fold (+) "" |> sprintf "(%s)"

  let private (>~) elem group =
    match group with
    | Syntax.Group elems -> Syntax.Group (elem :: elems)
    | _ -> failwith <| sprintf "Cannot add element to group '%A'" group

  let private selectSyntax token previous =
    match token with
    | "-" ->
      match previous with
      | Some (Value _ | Group _) -> Function Subtract
      | Some (Function _) | None -> Negate
      | Some other -> failwith <| sprintf "Unexpected '-' after '%s'" (show other)
    | "(" ->
      match previous with
      | Some (Function _ | Negate) | None -> GroupOpen
      | Some other -> failwith <| sprintf "Cannot begin a group after '%s'" (show other)
    | ")" ->
      match previous with
      | Some (Value _ | Group _) -> GroupClose
      | None -> failwith <| "Mismatched parentheses - expected '('"
      | Some other -> failwith <| sprintf "Cannot close a group after '%s'" (show other)
    | other ->
      match other |> Operator.tryGetOperator with
      | Some operator ->
        match previous with
        | Some (Value _ | Group _) -> Function operator
        | _ -> failwith <| sprintf "Cannot parse expression at '%s'" other
      | _ -> Value (double other)

  let private lex str =
    let rec lex groups top previous remaining =
      match remaining with
      | (token :: rest) ->
        let InfixSyntax = selectSyntax token previous
        match InfixSyntax with
        | GroupOpen -> lex (top :: groups) (Syntax.Group []) None rest
        | GroupClose ->
          match groups with
          | (groupHead :: groupTail) -> lex groupTail (top >~ groupHead) (Some top) rest
          | [] -> failwith "Mismatched parentheses - expected '('"
        | _ -> lex groups (InfixSyntax >~ top) (Some InfixSyntax) rest
      | _ -> top :: groups
    if tokenRegex.Replace(str, "").Trim() <> "" then
      printfn "err: %s"  (tokenRegex.Replace(str, "").Trim())
      failwith <| sprintf "Unexpected tokens in input - %s" str
    let tokens = tokenize str
    let groups = lex [] (Syntax.Group []) None tokens
    if groups |> Seq.length = 1 then
      let rec reverseTree = function
      | Group elems -> Syntax.Group (elems |> List.rev |> List.map reverseTree)
      | other -> other
      reverseTree groups.[0]
    else
      failwith "Mismatched parentheses - expected ')'"

  let rec private buildExpression op rhs = function
    | Expression (lhs2, op2, rhs2) as lhs ->
      if (op |> Operator.toPrecedence) <= (op2 |> Operator.toPrecedence) then
        Expression (lhs, op, rhs)
      else
        Expression (lhs2, op2, rhs2 |> buildExpression op rhs)
    | lhs -> Expression (lhs, op, rhs)

  let parse str =
    let rec parseSyntax syntax =
      let rec (|SignedGroup|_|) elems =
        let (sign, rest) =
          match elems with
          | (Negate :: xs) -> (Negative, xs)
          | xs -> (Positive, xs)
        match rest with
        | (Value value :: xs) -> Some (ExpressionTree.Group (sign, ExpressionTree.Value value), xs)
        | (Group _ as group :: xs) -> Some (ExpressionTree.Group (sign, parseSyntax group), xs)
        | _ -> None
      let rec parseSyntax expr remaining =
        match remaining with
        | (Function op :: rest) ->
          match rest with
          | SignedGroup (rhs, rest2) -> parseSyntax (expr |> buildExpression op rhs) rest2
          | (other :: _) -> failwith <| sprintf "Expecting a value or expression, found '%s'" (other |> show)
          | _ -> failwith <| sprintf "Unexpected end of expression: '%s'" (expr |> Expression.show)
        | (other :: _) -> failwith <| sprintf "Expecting a function, found '%s'" (other |> show)
        | _ -> expr

      match syntax with
      | Value value -> ExpressionTree.Value value
      | Group elems ->
        match elems with
        | SignedGroup (lhs, rest) -> parseSyntax lhs rest
        | (other :: _) -> failwith <| sprintf "Expecting a value or expression, found '%s'" (other |> show)
        | _ -> failwith "Expression cannot be empty"
      | other -> failwith <| sprintf "Expecting a value or expression, found '%s'" (other |> show)
    str |> lex |> parseSyntax
