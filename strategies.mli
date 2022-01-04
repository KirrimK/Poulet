(* Strategies.mli *)

val add_hyp : proposition -> Proof.t -> Proof.t

val add_remainder : proposition -> Proof.t -> Proof.t

val remainderLines : Proof.t -> int

val getFirstRemainder : Proof.t -> proposition

val nettoyer : Proof.t -> Proof.t

(* Stratégies appliquables à un problème mathématique *)

val falseHypo : int -> Proof.t -> bool * Proof.t

val andsplit : Proof.t -> bool * Proof.t

val andSplitHypo : int -> Proof.t -> bool * Proof.t

val orSplit : bool -> Proof.t -> bool * Proof.t

val orSplitHypo : bool -> int -> Proof.t -> bool * Proof.t

val intro : Proof.t -> bool * Proof.t

val exact : int -> Proof.t -> bool * Proof.t

val apply : int -> Proof.t -> bool * Proof.t

val applyInHyp : bool -> int -> int -> Proof.t -> bool*Proof.t

val assumption : Proof.t -> bool * Proof.type
