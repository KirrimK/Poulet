(* notsoquickcheck.mli *)

val propAleatoire : int -> Proposition.t;;

val add_rand_goal : int -> Proof.t -> bool * Proof.t;;

val add_rand_cont : int -> int -> Proof.t -> bool * Proof.t;;
