open String_calculator.Calculator

let calc = new calculator
let input = Sys.argv.(1)
let res = calc#sum input;;

Printf.printf "%d\n" res
