
type minmax = Minimizing | Maximizing

(** This function will generate a list (of size n) containing bit strings that are 3 codons in
 *  length and consist solely of zeros. *)
val gen_init_pop : int -> string list

(** This function will mutate the bitstring provided, flipping the bit at the index provided.
 *  Recall that strings a zero indexed.  *)
val mutate : string  -> int -> string

val crossover: string * string -> int -> string * string

val get_best_fitness: (string * float) list -> minmax -> (string * float)

(**
 * Roulette wheel selection using stochastic acceptance.
 *
 * This function accepts and returns a list of tuples that contain language instances and their associated fitness costs.
 *)
val selection: minmax -> (string * float) list -> (string * float) list


(**
 * Standard genetic algorithm - applies crossover, mutation at 80/12
 * 
 *)
val standard_ga: int -> int -> string list -> string list
