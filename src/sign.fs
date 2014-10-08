module FsCalculator.Sign

type Sign = Positive | Negative

let show = function
  | Sign.Positive -> ""
  | Sign.Negative -> "-"
