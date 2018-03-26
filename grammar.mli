type production_rule = string * string list;;

type grammar = 
    {
        non_terminals: string list;
        terminals: string list;
        start: string;
        production_rules: production_rule list;
    };;

exception InvalidInstance

(*Parsing*)
val parse_json: Yojson.Basic.json -> grammar

val parse_grammar_file: string -> grammar

val split_on_first_non_terminal: string list -> string list -> string list * string list

val reduce_non_terminal: string -> int -> production_rule-> string list

val get_rules: grammar -> string -> production_rule

(* bit string -> applied grammar -> fully parsed language instance
 *
 * This function accepts a bit string and a grammar to apply and returns a fully parsed 
 * language instance of the grammar passed as a parameter. *)
val parse_language_instance: string ->  grammar -> string list

(*Printing*)
val print_rules : production_rule list -> unit
val print_grammar: grammar -> unit
