let gen_init_pop n = 
    let rec aux l = 
        if List.length l < n then
            let a = "000000000000000000000000" in 
            aux (a :: l)
        else l 
     in aux [];;

