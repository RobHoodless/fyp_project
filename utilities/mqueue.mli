 type 'a queue
 exception EmptyQueue

 val empty : 'a queue

 val isEmpty : 'a queue -> bool

 val enqueue : 'a -> 'a queue -> 'a queue

 val dequeue : 'a queue -> 'a queue

 val front : 'a queue -> 'a

 val map : ('a -> 'b) -> 'a queue -> 'b queue

 val to_list : 'a queue -> 'a list
