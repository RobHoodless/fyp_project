module S = Mstack
module Q = Mqueue

exception UnknownToken

type associativity = Left | Right;;

let is_int s =
    try ignore (int_of_string s); true
    with _ -> false

let is_float s =
    try ignore (float_of_string s); true
    with _ -> false

let is_numeric s = 
    is_int s || is_float s

let is_operator s =
    match s with
    | "*" -> true
    | "/" -> true
    | "+" -> true
    | "-" -> true
    | "sin" -> true
    | "cos" -> true
    | "tan" -> true
    | "log" -> true
    | "exp" -> true
    | _ -> false

let is_function s =
    match s with
    | "sin" -> true
    | "cos" -> true
    | "tan" -> true
    | "log" -> true
    | "exp" -> true
    | _ -> false

let precedence op = 
    match op with 
    | "sin" -> 6
    | "cos" -> 6
    | "tan" -> 6
    | "log" -> 6
    | "exp" -> 6
    | "*" -> 3
    | "/" -> 3
    | "+" -> 2
    | "-" -> 2
    | _ -> -1;; (* Replace this with optional return value *)

let association op =
    match op with 
    | "^" -> Right
    | _ -> Left;;

(*  This function pops operators off the operator stack and onto the output queue while the 
 *  following conditions are true: 
 *      ((There is an operator at the top of the operator stack with greater precedence than
 *      current operator) 
 *    OR (The operator at the top of the operator stack has equal precedence and the 
 *        operator is left associative)) 
 *    AND (The operator at the top of the stack is not a left bracket).
 *)
let pop_from_stack curr_op op_stack out_queue = 
    let rec aux op_stack' out_queue' =
        if (S.isEmpty op_stack') != true then
            let expr1 = precedence (S.top op_stack') > precedence curr_op in 
            let expr2 = precedence (S.top op_stack') = precedence curr_op && 
                association curr_op = Left in 
            let expr3 = (S.top op_stack') <> "(" in 
            if (((expr1 = true) || (expr2 = true)) && (expr3 = true)) then
                aux (S.pop op_stack') (Q.enqueue (S.top op_stack') out_queue')
            else
                (op_stack', out_queue') 
        else 
            (op_stack', out_queue') in
    aux op_stack out_queue

(* Should add mechanism to handle raising failures when a left parenthesis is not encountered *)
let pop_until_left_paren op_stack out_queue = 
    let rec aux op_stack' out_queue' = 
        if (S.top op_stack') <> "(" then
            aux (S.pop op_stack') (Q.enqueue (S.top op_stack') out_queue')
        else 
            ((S.pop op_stack'), out_queue') in
    aux op_stack out_queue

let pop_remainder op_stack out_queue = 
    let rec aux op_stack' out_queue' = 
        if (S.isEmpty op_stack') != true then
            aux (S.pop op_stack') (Q.enqueue (S.top op_stack') out_queue')
        else
            out_queue' in
    aux op_stack out_queue

let evaluate (expr: string list) : string Q.queue = 
    let rec aux input out_queue op_stack = 
       match input with 
       | h :: t ->
               if (is_numeric h) then 
                  aux t (Q.enqueue h out_queue) op_stack
               else if (is_operator h) then
                   let (op_stack', out_queue') = pop_from_stack h op_stack out_queue in
                   aux t out_queue' (S.push h op_stack') 
               else if h = "(" then
                   aux t out_queue (S.push h op_stack) 
               else if h = ")" then 
                   let (op_stack', out_queue') = pop_until_left_paren op_stack out_queue in 
                   (* Check if the top of the operator stack is a function and pop if so *)
                   if ((S.isEmpty op_stack' = false) && is_function (S.top op_stack')) then
                       aux t (Q.enqueue (S.top op_stack') out_queue') (S.pop op_stack')
                   else
                       aux t out_queue' op_stack'
               else
                   raise UnknownToken

       | [] ->  pop_remainder op_stack out_queue in
    aux expr Q.empty S.empty
