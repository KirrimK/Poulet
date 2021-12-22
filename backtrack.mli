(* backtrack.mli *)

val getStratList : Strategies.proof -> ((Strategies.proof -> bool * Strategies.proof) * string) list

val backtrack: Strategies.proof -> bool -> bool
