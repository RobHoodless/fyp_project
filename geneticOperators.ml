
type minmax = Minimizing | Maximizing
type scale = Up | Down

let gen_bitstring () = 
    let rec aux s = 
        if String.length s = 256 then
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

let crossover t p = 
    let (a,b) = t in
    let a1 = String.sub a 0 (p+1) in
    let a2 = String.sub a (p+1) ((String.length a) - (p+1)) in
    let b1 = String.sub b 0 (p+1) in
    let b2 = String.sub b (p+1) ((String.length b) - (p+1)) in
    (a1 ^ b2, b1 ^ a2) 

let get_best_fitness l minmax = 
    let rec aux l' acc = 
        match l' with 
        | [] -> acc
        | (a,b) :: t ->
                let (a',b') = acc in 
                match minmax with 
                | Maximizing -> 
                        if b > b' then
                            aux t (a, b) 
                        else
                            aux t (a', b') 
                | Minimizing ->
                        if b < b' then
                            aux t (a,b)
                        else
                            aux t (a', b') in 
    aux l (List.hd l)

let invert_and_scale_fitness l scale =
    let rec aux l' acc = 
        match l' with 
        | (i, fi) :: t ->
                let fi_adj = match scale with 
                | Up -> (10000.0 *. (1.0 /. fi))
                | Down -> ((1.0 /. fi) /. 100000.0) in
                aux t ((i, fi_adj) :: acc)
        | [] -> acc in
    aux l []


(* Assumption: Selection will not be applied if goal fitness reached.
 * i.e no division by zero can occur.*)
let selection minmax l = 
    (*RWS uses maximizing fitness values, so invert if we have minimizing ones *)
    let l_adj = 
        match minmax with 
        | Minimizing -> invert_and_scale_fitness l Up
        | Maximizing -> l in
    let (imax, fmax) = get_best_fitness l_adj Maximizing in
    let rec aux l' = 
        if List.length l' = List.length l_adj then
            l' 
        else
            let () = Random.self_init () in
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
    (* if the fitness cost was inverted, invert back now *)
    let l' = aux [] in
    match minmax with
    | Minimizing -> invert_and_scale_fitness l' Down
    | Maximizing -> l'

let standard_ga c_prob m_prob pop = 
    let rec aux pop' acc =
        let () = Random.self_init () in 
        let x = Random.int 101 in
        if x < (c_prob) then
            (* apply crossover *)
            match pop' with
            | a :: b :: t ->
                    (* Generate crossover point *)
                    let x = Random.int ((String.length a) - 1) in 
                    let (a', b') = crossover (a,b) x in
                   aux t (a' :: b' :: acc)
            | h :: [] -> 
                   (* Not enough elements to apply crossover, so just apply reproduction *)
                    aux [] (h :: acc)
            | [] -> acc
        else if x < ( m_prob) then
            (* apply mutation *)
            match pop' with
            | h :: t -> 
                    let x = Random.int (String.length h) in
                    let h' = mutate h x in
                   aux t (h' :: acc)
            | [] -> acc 
        else 
            match pop' with 
            | h :: t -> 
                    aux t (h :: acc) 
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
