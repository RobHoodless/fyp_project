(** This function will generate a list (of size n) containing bit strings that are 3 codons in
 *  length and consist solely of zeros. *)
val gen_init_pop : int -> string list

(** This function will mutate the bitstring provided, flipping the bit at the index provided.
 *  Recall that strings a zero indexed.  *)
val mutate : string  -> int -> string

val crossover: string * string -> int -> string * string
