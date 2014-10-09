namespace Calculator

type ExpressionSign = Positive | Negative

module Sign =
  let show = function
    | Positive -> ""
    | Negative -> "-"
