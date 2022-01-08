(* backtrack.mli *)

val backtrack:  int -> (int -> Proof.t -> string) -> Proof.t -> bool * Proof.t

