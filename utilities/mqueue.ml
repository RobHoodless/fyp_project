module S = Mstack

type 'a queue = ('a S.stack * 'a S.stack)
exception EmptyQueue

let empty: 'a queue = (S.empty, S.empty)

let isEmpty ((s1,s2): 'a queue) = 
    S.isEmpty s1 && S.isEmpty s2

let enqueue (x : 'a) ((s1, s2) : 'a queue) : 'a queue = 
    ((S.push x s1), s2)

(* Refactor this function to stop using a tuple on the recursive auxiliary function. *)
let rev (s: 'a S.stack) : 'a S.stack = 
    let rec aux ((prev : 'a S.stack), (curr : 'a S.stack)) : 'a S.stack = 
        if S.isEmpty prev
        then curr
        else aux (S.pop prev, (S.push (S.top prev) curr))
    in 
    aux (s, S.empty)

let dequeue ((s1,s2) : 'a queue) : 'a queue = 
    if S.isEmpty s2
    then try (S.empty, S.pop (rev s1))
        with S.EmptyStack -> raise EmptyQueue
    else (s1, S.pop s2)

let front ((s1, s2) : 'a queue) : 'a = 
    if S.isEmpty s2 
    then try S.top (rev s1)
        with S.EmptyStack -> raise EmptyQueue
    else S.top s2

let map (f: 'a -> 'b) ((s1,s2) : 'a queue) : 'b queue = 
    (S.map f s1, S.map f s2)
