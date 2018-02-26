let to_list s = 
    let rec aux i  =
        if i = (String.length s) then []
        else (String.sub s i 1) :: (aux (i+1) ) in 
    aux 0;;

let from_list l = 
    let rec aux s l = 
        match l with
        | [] -> s ^ ""
        | h :: t -> aux (s ^ h) t in
    aux "" l;;
