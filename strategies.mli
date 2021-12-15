(* Strategies.mli *)

type proposition

type hypothesis

type proof

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
