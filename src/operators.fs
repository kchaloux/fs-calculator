module FsCalculator.Operators

open FsCalculator.Sign

type Operator = Exponent | Multiply | Modulus | Divide | Add | Subtract

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
