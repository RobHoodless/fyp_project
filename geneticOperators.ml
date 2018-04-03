module G = Grammar 

type minmax = Minimizing | Maximizing
type scale = Up | Down

let gen_bitstring () = 
    let rec aux s = 
        if String.length s = 32 then
            s
        else
            let () = Random.self_init () in
            let x = Random.int 100 in 
            if x < 49 then
                aux (s ^ "1") 
            else
                aux (s ^ "0") in 
    aux ""


            
let gen_init_pop n = 
    let rec aux l = 
        if List.length l < n then
            let a = gen_bitstring () in
            aux (a :: l)
        else l in 
    aux []

let flip_int n = 
    match n with
        | "0" -> "1"
        | "1" -> "0"
        | _ -> "0"

let mutate s i = 
    let rec aux s' j =
        if j = String.length s then s'
        else if j = i then
            aux (s' ^ flip_int (String.sub s j 1)) (j+1) 
        else
            aux (s' ^ (String.sub s j 1)) (j+1)  in
    aux "" 0

let apply_mutation s p = 
    let () = Random.self_init () in 
    let rec aux s' i = 
        let x = Random.int 101 in
        if (String.length s') = (String.length s) then
            s'
        else if x < p then
            aux (s' ^ flip_int (String.sub s i 1)) (i+1)
        else
            aux (s' ^ (String.sub s i 1)) (i+1) in 
    aux "" 0

let duplicate s = 
    if (String.length s) < 8 then 
        s
    else
        (* Generate starting position for duplication *)
        let m = (String.length s) / 8 in
        let () = Random.self_init () in
        let c = Random.int m in
        (* Multiply by 8 to get the start *) 
        let start = c * 8 in

        (* Generate number of codons to duplicate *) 
        let d = Random.int m in 
        if (start + (d*8)) < (String.length s) then
            let s' = String.sub s start (d*8) in
            s ^ s' 
        else
            let s' = String.sub s start ((String.length s) - start) in
            s ^ s' 

let apply_duplication s d_prob = 
    let () = Random.self_init () in 
    let x = Random.int 101 in
    if x < d_prob then
        duplicate s 
    else 
        s


let crossover t p1 p2 = 
    let (a,b) = t in
    let a1 = String.sub a 0 (p1+1) in
    let a2 = String.sub a (p1+1) ((String.length a) - (p1+1)) in
    let b1 = String.sub b 0 (p2+1) in
    let b2 = String.sub b (p2+1) ((String.length b) - (p2+1)) in
    (a1 ^ b2, b1 ^ a2) 

let apply_operators pop acc c_prob m_prob d_prob = 
        let () = Random.self_init () in 
        let x = Random.int 101 in
        if x < (c_prob) then
            (* apply crossover *)
            match pop with
            | a :: b :: t ->
                    (* Generate crossover point *)
                    let x = Random.int ((String.length a) - 1) in 
                    let y = Random.int ((String.length b) - 1) in
                    let (a', b') = crossover (a,b) x y in
                    (* After applying recombination, apply mutation based on m_prob. *)
                    let a'' = apply_mutation a' m_prob in
                    let b'' = apply_mutation b' m_prob in 
                    let ad = apply_duplication a'' d_prob in
                    let bd = apply_duplication b'' d_prob in
                    let acc' = ad :: bd :: acc in
                    (t, acc')
            | h :: [] -> 
                   (* Not enough elements to apply crossover, so just pass element onto next generation.*)
                    ([], h :: acc)
            | [] -> ([], acc)
        else 
            match pop with 
            | h :: t -> 
                    (t, h :: acc) 
            | [] -> ([], acc) 

let sort_array arr = 
    let comp x y = 
        let (a, fa) = x in
        let (b, fb) = y in
        compare fa fb in
     Array.sort comp arr

let get_best_fitness l minmax = 
    let arr = Array.of_list l in
    let () = sort_array arr in
    match minmax with 
    | Minimizing -> 
            Array.get arr 0 
    | Maximizing ->
           Array.get arr ((Array.length arr) - 1) 

let get_best_pair l init = 
    let comp x y = 
        let (a, fa) = x in
        let (b, fb) = y in
        compare fa fb in
    let l' = List.sort comp l in
    ((List.nth l' 0), (List.nth l' 1))

let get_min_and_max pop = 
    let arr = Array.of_list pop in 
    let comp x y = 
        let (a, fa) = x in
        let (b, fb) = y in
        compare fa fb in
    let () = Array.sort comp arr in
    let min = Array.get arr 0 in
    let max = Array.get arr ((Array.length arr) - 1) in
    (min, max)


let normalise_and_invert pop minmax = 
    let ((min, fmin), (max, fmax)) = get_min_and_max pop in
    let fdiff = fmax -. fmin in
    let rec aux pop' acc = 
        match pop' with 
        | (h, fh) :: t ->
                let fh_norm = if (fdiff = 0.0) then 0.5 else (fh -. fmin) /. fdiff in
                let fh_norm' = match minmax with
                | Minimizing -> (1.0 -. fh_norm)
                | Maximizing -> fh_norm in
                aux t ((h, fh_norm') :: acc)
        | [] -> acc in
    aux pop []

let scale_fitness pop = 
    let a = 1.0 in
    let b = 0.3 in
    let rec aux pop' acc = 
        match pop' with 
        | (h, fh) :: t -> 
                let fh_scaled = (a *. fh) -. b in
                aux t ((h, fh_scaled) :: acc)
        | [] -> acc in
    aux pop []

let get_raw_fitness s l = 
    let find_predicate p = 
        let (a, fa) = p in
        a = s in
    let (a, fa) = List.find find_predicate l in 
    fa

let apply_fitness fitness grammar s minmax = 
    try 
        fitness (G.parse_language_instance s grammar)
    with
        G.InvalidInstance ->
            match minmax with 
            | Minimizing ->  1000.0
            | Maximizing -> 0.0

(* Assumption: Fitness values are raw fitness. *)
let steady_state_selection grammar pop fitness minmax = 
    let arr = Array.of_list pop in 
    let () = sort_array arr in
    let ap = Array.get arr 0 in
    let bp = Array.get arr 1 in
    let (a, fa) = ap in
    let (b, fb) = bp in
    let x = Random.int ((String.length a) - 1) in 
    let y = Random.int ((String.length b) - 1) in
    let (ac, bc) = crossover (a,b) x y in
    let am = apply_mutation ac 1 in
    let bm = apply_mutation bc 1 in
    let a' = apply_duplication am 1 in
    let b' = apply_duplication bm 1 in
    let fa' = apply_fitness fitness grammar a' minmax in
    let fb' = apply_fitness fitness grammar b' minmax in
    let ap' = (a', fa') in
    let bp' = (b', fb') in
    let () = Array.set arr ((Array.length arr) - 1) ap' in
    let () = Array.set arr ((Array.length arr) - 2) bp' in 
    Array.to_list arr

(* Assumption: Selection will not be applied if goal fitness reached.
 * i.e no division by zero can occur.*)
let selection minmax l = 
    let () = Random.self_init () in
    (*RWS uses maximizing fitness values, so invert if we have minimizing ones *)
    let l_norm = normalise_and_invert l minmax in 
    let l_adj = scale_fitness l_norm in 
    let (imax, fmax) = get_best_fitness l_adj Maximizing in
    let rec aux l'  = 
        if List.length l' = List.length l_adj then
            l' 
        else
            (* Randomly select an individual (i) - selection is not influenced by fitness value *)
            let r = Random.int (List.length l_adj) in 
            let (i, fi) = List.nth l_adj r in 
            (* Generate a value between 0 and fmax *)
            let x = Random.float fmax in 
            if x < fi then
                (* if x is in the range [0-fi] then accept the selection *)
                aux ((i, fi) :: l') 
            else
                (* otherwise reject the selection *)
                aux l' in
    (* Elitism test: Add the best individual to the population everytime anyway.*)
    let p = (imax, fmax) in
    aux (p :: p:: p :: p :: p :: [])

let standard_ga c_prob m_prob d_prob pop = 
    let rec aux pop' acc =
        match pop' with
        | h :: t -> 
            let (pop'', acc') = apply_operators pop' acc c_prob m_prob d_prob in
            aux pop'' acc' 
        | [] -> acc in
    aux pop [] 

(*let () = 
    let (a,b) = crossover ("0011", "1100") 1 in
    Printf.printf "(%s, %s)" a b 
*)
(*
let () = 
    let a = selection Minimizing [("0000", 1000.0);("0000", 1000.0);"0000", 1000.0] in
    Printf.printf "%f" b
    let rec aux a' =
        match a' with 
        | [] -> ()
        | (x,y) :: t -> 
                let () = Printf.printf "(%s, %f) " x y in 
                aux t in

    aux a
    *)
