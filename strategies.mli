(* Strategies.mli *)

val fail : Proof.t -> bool * Proof.t

val intro : Proof.t -> bool * Proof.t

val split : Proof.t -> bool * Proof.t

val hyp_split : int -> Proof.t -> bool * Proof.t

val left : Proof.t -> bool * Proof.t

val right : Proof.t -> bool * Proof.t

val hyp_left : int -> Proof.t -> bool * Proof.t

val hyp_right : int -> Proof.t -> bool * Proof.t

val false_hyp : int -> Proof.t -> bool * Proof.t

val exact : int -> Proof.t -> bool * Proof.t

val assumption : Proof.t -> bool * Proof.t

val apply : int -> Proof.t -> bool * Proof.t

val applyInHyp : bool -> int -> int -> Proof.t -> bool * Proof.t
