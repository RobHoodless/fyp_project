module Q = Mqueue
module Shunt = ShuntingYard 
module Eval = Evaluation

let parse_json_data json = 
    let open Yojson.Basic.Util in 
    let rec aux acc i =
        if i < 21 then
            let p = json |> member (string_of_int i) in
            let a = p |> member "a" |> to_string |> float_of_string in 
            let b = p |> member "b" |> to_string |> float_of_string in 
            aux ((a,b) :: acc) (i + 1)
        else 
            List.rev acc in
    aux [] 1

let parse_data_file file = 
    parse_json_data (Yojson.Basic.from_file file)

let strip_x expr x = 
    let rec aux expr' acc = 
        match expr' with 
        | h :: t ->
                let h' = Str.global_replace (Str.regexp "X") (string_of_float x) h in 
                aux t (h' :: acc)
        | [] -> List.rev acc in
    aux expr []

let print_expression expr = 
    let rec aux expr' s = 
        match expr' with 
        | h :: t -> aux t (s ^ " " ^ h) 
        | [] -> s in
    let s = aux expr "" in
    let s' = Printf.sprintf "The expression is: %s" s in
    print_endline s'


let fitness values expr = 
    let rec aux values' acc =
        match values' with 
        | (a, b) :: t ->
                let expr' = strip_x expr a in
                (*let () = print_expression expr' in*)
                let sy_expr = Q.to_list (Shunt.evaluate expr') in
                let r = Eval.evaluate sy_expr in
                if (compare r nan = 0 || compare r infinity = 0 || compare (r *. -1.0) infinity = 0) then
                    1000.0
                else
                    aux t (acc +. abs_float (b -. r))
        | [] ->
                acc in 
    let fcost = aux values 0.0 in
    if (fcost < 0.00001) then
        0.0 
    else 
        fcost

