module S = Mstack
module Q = Mqueue
module Shunt = ShuntingYard

(* TODO: Refactor these utility functions into a separate module. *)
let is_int s =
    try ignore (int_of_string s); true
    with _ -> false

let arity s = 
    match s with
    | "*" -> 2
    | "/" -> 2
    | "+" -> 2
    | "-" -> 2
    | "log" -> 1
    | "sin" -> 1
    | "cos" -> 1
    | "tan" -> 1
    | "exp" -> 1
    | _ -> 0

(* List of functions that map to the operations within the expressions. All of the functions
 * accept a list of floats and return a single float (result) *)

let add a b =
    a +. b

let subtract a b = 
    a -. b 

let multiply a b =
   a *. b 

let divide a b = 
   a /. b

(* Temporary function to have to make type check work for function mapping operations to 
 * functions. Need to eventually change to make use of optional return value. *)
let id1 a b = 
    0.0
let id2 a =
    0.0

let get_two_arg_func s = 
    match s with 
    | "*" -> multiply
    | "/" -> divide
    | "+" -> add
    | "-" -> subtract
    | _ -> id1

let get_one_arg_func s = 
    match s with 
    | "sin" -> sin
    | "cos" -> cos
    | "tan" -> tan
    | "log" -> log
    | "exp" -> exp
    | _ -> id2


let is_float s =
    try ignore (float_of_string s); true
    with _ -> false

let is_numeric s = 
    is_int s || is_float s


(* This function assumes that only one and two argument functions are supported. More arguments
 * could be supported by creating a list of the arguments to be applied and recursively 
 * currying the application function and returning the result. I'll do this later if I decide
 * to support functions with N arguments. This is the simpler method right now though. *)
let handle_operator operator op_stack = 
    if arity operator = 1 then
        let a = S.top op_stack in 
        let op_stack' = S.pop op_stack in
        S.push ((get_one_arg_func operator) a) op_stack'
    else
        let b = S.top op_stack in 
        let op_stack' = S.pop op_stack in
        let a = S.top op_stack' in 
        let op_stack'' = S.pop op_stack' in 
        S.push ((get_two_arg_func operator) a b) op_stack''

let evaluate (expr: string list) : float = 
    let rec aux input op_stack = 
        match input with 
        | h :: t -> 
                if (is_numeric h) then
                    aux t (S.push (float_of_string h) op_stack)
                else
                    let op_stack' = handle_operator h op_stack in 
                   aux t op_stack' 
        | [] -> S.top op_stack in
    aux expr S.empty


