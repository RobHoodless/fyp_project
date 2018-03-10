module S = Mstack
module Q = Mqueue

(* Convert infix expression to postfix expression *)
val evaluate: string list -> string Q.queue
