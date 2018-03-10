type 'a stack = 'a list
exception EmptyStack

let empty : 'a stack = []

let isEmpty (l : 'a list) : bool = l = []

let push (x : 'a) (l : 'a stack) : 'a stack = x :: l

let pop (l : 'a stack) : 'a stack = 
    match l with 
    [] -> raise EmptyStack
    | h :: t -> t

let top (l : 'a stack) : 'a = 
    match l with
    [] -> raise EmptyStack
    | h :: t -> h

let map (f : 'a -> 'b) (l : 'a stack) : 'b stack = List.map f l 
