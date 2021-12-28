(* backtrack.mli *)

val backtrack: Strategies.proof -> bool -> (int -> Strategies.proof -> string) -> bool * Strategies.proof
