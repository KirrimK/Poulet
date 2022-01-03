(* Strategies.mli *)

type proposition

type hypothesis

type proof

val empty_proof: proof

val p_name : string -> proposition

val p_true : proposition

val p_false : proposition

val ( => ) : proposition -> proposition -> proposition

val ( ^ ) : proposition -> proposition -> proposition

val ( $ ) : proposition -> proposition -> proposition

val p_not : proposition -> proposition

exception Invalid_Input

val propAleatoire : int -> proposition

val make_prop : string list -> proposition

val add_hyp : proposition -> proof -> proof

val add_remainder : proposition -> proof -> proof

val getAllHypoIds : proof -> int list

val getRootOfProp : proposition -> string

val getHypList : proof -> hypothesis list

val getRemainder : proof -> proposition list

val remainderLines : proof -> int

val getFirstRemainder : proof -> proposition

val getPropOfHyp : int -> proof -> proposition

val isProven : proof -> bool

val nettoyer : proof -> proof

(* Stratégies appliquables à un problème mathématique *)

val falseHypo : int -> proof -> bool * proof

val andsplit : proof -> bool * proof

val andSplitHypo : int -> proof -> bool * proof

val orSplit : bool -> proof -> bool * proof

val orSplitHypo : bool -> int -> proof -> bool * proof

val intro : proof -> bool * proof

val exact : int -> proof -> bool * proof

val apply : int -> proof -> bool * proof

val assumption : proof -> bool * proof

val prop_iter : (string -> 'a) -> 'a -> 'a -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> proposition -> 'a

val foncgen_hypo : (int -> 'a) -> (proposition -> 'b) -> hypothesis -> 'a * 'b
