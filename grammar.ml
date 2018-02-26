type production_rule = string * string list;;

type grammar = 
    {
        non_terminals: string list;
        terminals: string list;
        start: string;
        production_rules: production_rule list;
    };;

let parse_production_rules non_terminals json =
    let () = Printf.printf "parse production rules\n" in 
    let rec aux non_terminals' p_rules =
    let open Yojson.Basic.Util in 
        match non_terminals' with 
        | [] -> p_rules
        | h :: t -> aux t ((h, json |> member h |> to_list |> filter_string) :: p_rules) in
    aux non_terminals [];;

(* Append list l2 to the end of list l1 *)
let append l1 l2 = 
    let rec aux acc l1 l2 =
        match l1, l2 with
        | [], [] -> List.rev acc
        | [], h :: t -> aux (h :: acc) [] t 
        | h :: t, l -> aux (h :: acc) t l in
    aux [] l1 l2;;

let parse_json json =
    let open Yojson.Basic.Util in 
    let json_grammar = json |> member "grammar" in
    let nt = json_grammar |> member "non_terminals" |> to_list |> filter_string in
    let t = json_grammar |> member "terminals" |> to_list |> filter_string in
    let s = json_grammar |> member "start" |> to_string in
    let p = parse_production_rules nt (json_grammar |> member "production_rules") in
        {
            non_terminals = nt;
            terminals = t;
            start = s;
            production_rules = p;
        };;

let parse_grammar_file file = 
    parse_json (Yojson.Basic.from_file file);;

(* This function splits a language instance when it encounters the leftmost non terminal. The 
 * return value is a tuple containing the list of elements of the string preceding the 
 * leftmost non terminal and a list of the remaining elements. Note that if no non terminal
 * is encountered this function will return the fully parsed language instance list and an empty
 * list. *)
let split_on_first_non_terminal language_instance non_terminals = 
    (* Accept l1 (list of terminals) l2 (list to start with first non terminal) *)
    let rec aux l1 l2 = 
        match l2 with
        | [] -> (List.rev l1,[])
        | h :: t ->
                if (List.mem h non_terminals) then
                    (List.rev l1, l2)
                else
                    (* Build list of encountered terminals (in reverse order). *)
                    aux (h :: l1) t in
    aux [] language_instance;;

(* 
 * This function returns the RHS of the chosen production rule based on the 
 * bitstring and codon index.
 *
 * This function works off the assumption that a bitstring is some multiple of 8. Would be 
 * better to accept codon length as a parameter
 *)
let reduce_non_terminal bitstring c_index p_rules = 
    let codon_value = Int32.to_int (Int32.of_string ("0b" ^ String.sub bitstring c_index 7)) in 
    let (k, v) = p_rules in 
    let rule_index = codon_value mod (List.length v) in
    let rule = List.nth v rule_index in 
    Core.Std.String.split rule ~on:';';;

(* 
 * This function will return all of the production rules associated with a non terminal
 *)
let get_rules grammar key = 
    let rules = grammar.production_rules in
    let rec aux p_rules  = 
       match p_rules with 
       | [] -> (key,[])
       | (a,b) :: t ->
               if a = key then
                   (a,b)
               else
                   aux t
    in aux rules;;

let replace_first_token replacement language_instance = 
    match language_instance with 
    | [] -> []
    | h :: t -> append replacement t;;

let parse_language_instance bitstring grammar =
    (*
     * Start recursive function.
     * call fn that returns two lists -> l1 is prior to leftmost Nt l2 is after.
     * Check if l2 is empty - if it is then the string is fully parsed.
     * choose production rule for leftmost Nt in l2 using the function that accepts bitstring,
     * codon index, and production rules. Fn should return RHS of production rule.
     * Pass l2 and RHS of production rule to Fn that replaces the Nt.
     * Create l' = append l1 l2
     * recurse with l' and codon index.
     *
     *
     *)
    let rec aux l c_index =
        let (l1, l2) = split_on_first_non_terminal l grammar.non_terminals in
        if List.length l2 = 0 then
            l1
        else
            let c_index' = if ( (c_index)  >= ((String.length bitstring))) then 0 else c_index in 
            let p_rule_rhs = reduce_non_terminal bitstring c_index' (get_rules grammar (List.hd l2)) in
            let l2' = replace_first_token p_rule_rhs l2 in
            let l' = append l1 l2' in
            let () = Printf.printf "%s\n" (String.concat " " l') in 
            aux l' (c_index' + 8) 
    in aux (grammar.start :: []) 0;;


(* Helper functions *)


(* Evil I/O stuff *)
let print_rules rules = 
    let rec aux rules' = 
        let open Printf in 
        match rules' with
        | [] -> ()
        | h :: t ->
                let (a,b) = h in
                let () = printf "%s: " a in
                let () = List.iter (printf "[%s] ") b in
                let () = printf "\n" in
                aux t in 
    aux rules;;


let print_grammar grammar = 
    let open Printf in 
    let () = printf "non_terminals:\n" in 
    let () = List.iter (printf "%s\n") grammar.non_terminals in 
    let () = printf "terminals: \n" in 
    let () = List.iter (printf "%s\n") grammar.terminals in 
    let () = printf "start symbol: %s\n" grammar.start in 
    let () = printf "Production rules: \n" in
    print_rules grammar.production_rules;;



let () = 
    (* Read the JSON file *)
    let g = parse_grammar_file "test_problems/symbolic_regression/grammar.json" in 
    let s = parse_language_instance "1110110110110100111111101110110100000110" g in 
    let () = Printf.printf "Final string: %s" (String.concat " " s) in
    let () = Printf.printf "\n" in ();;
