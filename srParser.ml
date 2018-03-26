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

let fitness values expr = 
    let rec aux values' acc =
        match values' with 
        | (a, b) :: t ->
                let expr' = strip_x expr a in
                let sy_expr = Q.to_list (Shunt.evaluate expr') in
                let r = Eval.evaluate sy_expr in
                aux t (acc +. abs_float (b -. r))
        | [] ->
                acc in 
    let fcost = aux values 0.0 in
    (* Accounting for rounding errors *)
    if fcost < 0.0001 then
        0.0 
    else
        fcost

