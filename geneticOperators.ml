let gen_init_pop n = 
    let rec aux l = 
        if List.length l < n then
            let a = "000000000000000000000000" in 
            aux (a :: l)
        else l in 
    aux [];;

let flip_int n = 
    match n with
        | "0" -> "1"
        | "1" -> "0"
        | _ -> "0";;

let mutate s i = 
    let rec aux s' j =
        if j = String.length s then s'
        else if j = i then
            aux (s' ^ flip_int (String.sub s j 1)) (j+1) 
        else
            aux (s' ^ (String.sub s j 1)) (j+1)  in
    aux "" 0;;

let crossover t p = 
    let (a,b) = t in
    let a1 = String.sub a 0 (p+1) in
    let a2 = String.sub a (p+1) ((String.length a) - p) in
    let b1 = String.sub b 0 (p+1) in
    let b2 = String.sub b (p+1) ((String.length b) - p) in
    (a1 ^ b2, b1 ^ a2);; 

