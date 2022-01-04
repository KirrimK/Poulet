type t

val getProp : t -> Proposition.t

val getId : t -> int

val newHypo : int -> Proposition.t -> t