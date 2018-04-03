module G = Grammar
module GeneticOp = GeneticOperators
module Sr = SrParser
module Ge = GrammaticalEvolution

let test () = 
    let g = G.parse_grammar_file "test_problems/symbolic_regression/grammar.json" in
    let fitness' = Sr.fitness (Sr.parse_data_file "test_problems/symbolic_regression/test_data.json") in
    let init_pop = GeneticOp.gen_init_pop 500 in
    let (i, f) = Ge.steady_state_evolve g init_pop fitness' 0.0 GeneticOp.Minimizing 200 in
    let () = Printf.printf "The best fitness cost was: %f\n" f in
    let () = Printf.printf "The bitstring was: %s\n" i in
    let () = Printf.printf "The parsed expression is: " in
    let () = List.iter (Printf.printf "%s ") (G.parse_language_instance i g) in 
    Printf.printf "%s\n" ""


let () = 
    test ()
