
module G = Grammar
module GeneticOp = GeneticOperators
module Sr = SrParser
module Ge = GrammaticalEvolution

let test () = 
    let g = G.parse_grammar_file "test_problems/symbolic_regression/grammar.json" in
    let fitness' = Sr.fitness (Sr.parse_data_file "test_problems/symbolic_regression/test_simple.json") in
    let init_pop = GeneticOp.gen_init_pop 500 in
    let sel = GeneticOp.selection in
    let ga = GeneticOp.standard_ga 50 1 1 in
    let (i, f) = Ge.evolve g init_pop sel ga fitness' 0.0 GeneticOp.Minimizing 250 in
    let () = Printf.printf "The best fitness cost was: %f\n" f in
    let () = Printf.printf "The bitstring was: %s\n" i in
    let () = Printf.printf "The parsed expression is: " in
    let () = List.iter (Printf.printf "%s ") (G.parse_language_instance i g) in 
    Printf.printf "%s\n" ""

let () = 
    test ()
