module G = Grammar
module GeneticOp = GeneticOperators

val apply_fitness_function: G.grammar -> (string list -> float) -> string list -> GeneticOp.minmax -> (string * float) list

val evolve: G.grammar -> string list -> (GeneticOp.minmax -> (string * float) list -> (string * float) list) -> (string list -> string list) -> (string list -> float) -> float -> GeneticOp.minmax -> int -> (string * float)

val steady_state_evolve: G.grammar -> string list -> (string list -> float) -> float -> GeneticOp.minmax -> int -> (string * float)
