module G = Grammar
module GeneticOp = GeneticOperators

let apply_fitness_function grammar fitness population minmax = 
    let rec aux pop' acc = 
       match pop' with 
       | [] -> acc
       | h :: t -> 
               try 
                   let expr = G.parse_language_instance h grammar in 
                   let fcost = fitness expr in 
                   aux t ((h, fcost) :: acc)
               with
                  G.InvalidInstance -> 
                      match minmax with 
                      | GeneticOp.Minimizing -> aux t ((h, 1000.0) :: acc)
                      | GeneticOp.Maximizing -> aux t ((h, 0.0) :: acc) in 
    aux population []

let print_population pop = 
    let rec aux pop' = 
        match pop' with
        | [] -> ()
        | (a,b) :: t ->
                let () = Printf.printf "(%s, %f) " a b in 
                aux t in
    aux pop

let steady_state_evolve grammar init_pop fitness goal_fitness minmax max_iters =
    let rec aux pop i = 
        let f_pop = apply_fitness_function grammar fitness pop minmax in
        let (imax, fmax) = GeneticOp.get_best_fitness f_pop GeneticOp.Minimizing in
        let s' = Printf.sprintf "The best fitness at gen %d is %f" i fmax in
        let () = print_endline s' in
        if fmax = goal_fitness then
            (imax, fmax) 
        else if i = (max_iters-1) then
            (imax, fmax) 
        else
            let f_pop' = GeneticOp.steady_state_selection grammar f_pop fitness minmax in
            let (l1,l2) = List.split f_pop' in
            aux l1 (i+1) in
    aux init_pop 0

let evolve grammar init_pop selection ga fitness goal_fitness minmax max_gen = 
    let rec aux pop' gen = 
        let f_pop = apply_fitness_function grammar fitness pop' minmax in 
        let (imax, fmax) = GeneticOp.get_best_fitness f_pop minmax in
        let (imin, fmin) = GeneticOp.get_best_fitness f_pop GeneticOp.Maximizing in
        let li = List.fold_left (^) "" (G.parse_language_instance imax grammar) in
        let s' = Printf.sprintf "Gen %d The best fitness is %f and worst fitness is %f (%s) " gen fmax fmin li in
        let () = print_endline s' in
        if (fmax = goal_fitness) then
            (imax, fmax) 
        else if gen = (max_gen) then
            (imax, fmax) 
        else
            let f_pop' = selection GeneticOp.Minimizing f_pop in
            let (l1,l2) = List.split f_pop' in 
            let pop'' = ga l1 in
            aux pop'' (gen+1) in
    aux init_pop 0 

