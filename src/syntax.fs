module FsCalculator.Syntax

open FsCalculator.Utility
open FsCalculator.Operators
open System.Text.RegularExpressions

type Syntax =
  | Negate
  | Value of double
  | Function of Operator
  | Group of Syntax list
  | GroupOpen
  | GroupClose

let private operatorPattern =
  sprintf @"[%s]"
    (operatorSymbols
    |> List.map (fun (_, symbol) -> "\\" + symbol)
    |> List.reduce (+))
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
  | Function op -> getSymbol op
  | Group elems -> elems |> List.map (show) |> List.fold (+) "" |> sprintf "(%s)"

let private (>~) (elem: Syntax) (group: Syntax) =
  match group with
  | Group elems -> Group (elem :: elems)
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
    match operatorSymbols |> Seq.tryFind (fun (_, symbol) -> symbol = other) with
    | Some (operator, _) ->
      match previous with
      | Some (Value _ | Group _) -> Function operator
      | _ -> failwith <| sprintf "Cannot parse expression at '%s'" other
    | _ -> Value (double other)

let lex str =
  let rec lex groups top previous remaining =
    match remaining with
    | (token :: rest) ->
      let syntax = selectSyntax token previous
      match syntax with
      | GroupOpen -> lex (top :: groups) (Group []) None rest
      | GroupClose ->
        match groups with
        | (groupHead :: groupTail) -> lex groupTail (top >~ groupHead) (Some top) rest
        | [] -> failwith "Mismatched parentheses - expected '('"
      | _ -> lex groups (syntax >~ top) (Some syntax) rest
    | _ -> top :: groups
  if tokenRegex.Replace(str, "").Trim() <> "" then
    failwith <| sprintf "Unexpected tokens in input - %s" str
  let tokens = tokenize str
  let groups = lex [] (Group []) None tokens
  if groups |> Seq.length = 1 then
    let rec reverseTree = function
    | Group elems -> Group (elems |> List.rev |> List.map reverseTree)
    | other -> other
    reverseTree groups.[0]
  else
    failwith "Mismatched parentheses - expected ')'"
