module FsCalculator.Utility

let showValue n = System.String.Format("{0:0.###############}", [|n|])
let flip f x y = f y x
