(* backtrack.mli *)

val backtrack: Proof.t -> bool -> (int -> Proof.t -> string) -> bool * Proof.t

