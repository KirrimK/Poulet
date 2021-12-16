(* Strategies.mli *)

type proposition

type hypothesis

type proof

val empty_proof: proof

exception Invalid_Input

val make_prop : string list -> proposition

val add_hyp : proof -> proposition -> proof

val add_remainder : proof -> proposition -> proof

val getAllHypoIds : proof -> int list

val getRootOfProp : proposition -> string

val remainderLines : proof -> int

val getFirstRemainder : proof -> proposition

val isRemainderTrue : proof -> bool

val splitProblem : proof -> proof list

(* Stratégies appliquables à un problème mathématique *)
val andsplit : proof -> bool * proof

val intro : proof -> bool * proof

val exact : int -> proof -> bool * proof

val apply : int -> proof -> bool * proof

val assumption : proof -> bool * proof

val prop_iter : (string -> 'a) -> 'a -> 'a -> ('a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> proposition -> 'a

val foncgen_hypo : (int -> 'a) -> (proposition -> 'b) -> hypothesis -> 'a * 'b
