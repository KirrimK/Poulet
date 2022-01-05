(* notsoquickcheck.mli *)

val prop_aleatoire = int -> int -> Proposition.t;;

val propAleatoire = int -> Proposition.t;;

val add_rand_goal = int -> Proof.t -> bool * Proof.t;;

val add_rand_cont = int -> int -> Proof.t -> bool * Proof.t;;

val get_rand_cont = int -> int -> bool * Proof.t;;

val rev_intro = int -> Proof.t -> bool * Proof.t;;

val rev_exact = int -> Proof.t -> bool * Proof.t;;

val rev_hyp_split = int -> int -> Proof.t -> bool * Proof.t;;

val rev_split = Proof.t -> bool * Proof.t;;

val rev_apply = int -> Proof.t -> bool * Proof.t;;

val reverse = Proof.t -> bool * Proof.t;;

val testMassif = unit -> unit;;
