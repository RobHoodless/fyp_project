module Shunt = ShuntingYard
module G = Grammar
module Eval = Evaluation
module Q = Mqueue
module S = Mstack
module GeneticOp = GeneticOperators

(* Test shunting yard algorithm *)
let test_sya () = 
    let expr = Shunt.evaluate ["(";"3";"+";"4";")";"*";"5"] in
    (Q.to_list expr) = ["3";"4";"+";"5";"*"] 

(* Test evaluation function *)
let test_eval () = 
    let result = Eval.evaluate ["3";"4";"+";"5";"*"] in 
    result = 35.0

(* Test grammar parsing *)
let test_grammar () = 
    let g = G.parse_grammar_file "test_problems/symbolic_regression/grammar.json" in
    let bitstring = "000001000000001100000010000001000000001100000010" in
    let expr = G.parse_language_instance bitstring g in
    expr = ["X";"+";"X"]

(* Crossover test*)
let test_crossover () = 
    let a = "000001101" in
    let b = "001010101" in
    let (a', b') = GeneticOp.crossover (a,b) 3 5 in 
    let a_exp = "0000101" in 
    let b_exp = "00101001101" in
    (a' = a_exp) && (b' = b_exp)

(* Run all of the tests *)
let tests = 
    [("Test: Shunting yard infix to postfix", test_sya);
     ("Test: Postfix evaluation", test_eval);
     ("Test: Grammar parsing", test_grammar);
     ("Test: Crossover", test_crossover)] 

let () = 
    let rec aux tests' = 
        match tests' with
        | [] -> 
                Printf.printf "All tests passed successfully \n" 
        | h :: t -> 
                let (s, test) = h in
                if (test ()) = true then
                    aux t
                else 
                    Printf.printf "%s: FAILED" s in
    aux tests
