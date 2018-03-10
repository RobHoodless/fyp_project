type 'a stack

exception EmptyStack

(* An empty stack*)
val empty: 'a stack

(* Check whether the stack is empty or not *)
val isEmpty: 'a stack -> bool

(* Returns a new stack with x pushed onto the top. *)
val push: 'a -> 'a stack -> 'a stack

(* Returns a new stack without the top element. *)
val pop: 'a stack -> 'a stack 

(* Returns the top element of the stack *)
val top: 'a stack -> 'a

(* Applies a mapping function to each element of the stack *)
val map : ('a -> 'b) -> 'a stack -> 'b stack
