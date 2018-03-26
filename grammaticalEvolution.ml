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
                      | GeneticOp.Minimizing -> aux t ((h, 25.0) :: acc)
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


let evolve grammar init_pop selection ga fitness goal_fitness minmax max_gen = 
    let rec aux pop' gen = 
        let f_pop = apply_fitness_function grammar fitness pop' minmax in 
        let (imax, fmax) = GeneticOp.get_best_fitness f_pop minmax in
        let () = Printf.printf "The best fitness at gen %d is %f\n" gen fmax in
        if fmax = goal_fitness then
            (imax, fmax) 
        else if gen = (max_gen-1) then
            (imax, fmax) 
        else
            let f_pop' = selection minmax f_pop in
            let (l1,l2) = List.split f_pop' in 
            let pop'' = ga l1 in
            aux pop'' (gen+1) in
    aux init_pop 0 

