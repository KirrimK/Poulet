(* backtrack.mli *)

val backtrack:  int -> (Proposition.t -> string) -> Proof.t -> bool * Proof.t

