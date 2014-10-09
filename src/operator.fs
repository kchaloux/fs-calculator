namespace Calculator

type MathOperator =
  | Exponent
  | Multiply
  | Modulus
  | Divide
  | Add
  | Subtract

module Operator =
  let private operatorSymbols =
    [ Exponent, "^"
      Multiply, "*"
      Modulus, "%"
      Divide, "/"
      Add, "+"
      Subtract, "-" ]

  let symbols = operatorSymbols |> List.map (fun (_, str) -> str)

  let show operator =
    match operatorSymbols |> Seq.tryFind (fun (op, _) -> op = operator) with
    | Some (_, symbol) -> symbol
    | _ -> failwith <| sprintf "Could not find a matching symbol for the operator '%A'" operator

  let tryGetOperator symbol =
    match operatorSymbols |> Seq.tryFind (fun (_, str) -> str = symbol) with
    | Some (operator, _ ) -> Some operator
    | _ -> None 

  let ofSymbol symbol =
    match tryGetOperator symbol with
    | Some operator -> operator
    | _ -> failwith <| sprintf "Could not find a matching operator for the symbol '%s'" symbol

  let toPrecedence = function
    | Exponent -> 3
    | Multiply -> 2
    | Modulus -> 2
    | Divide ->  2
    | Add -> 1
    | Subtract -> 1

  let toFunction = function
    | Exponent -> ( ** )
    | Multiply -> ( * )
    | Modulus -> ( % )
    | Divide -> ( / )
    | Add -> ( + )
    | Subtract -> ( - )
