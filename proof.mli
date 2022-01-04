type t

val empty : t

val get_hyps : t -> Proposition.t list

val get_goal : t -> Proposition.t list

val hyp_ids : t -> int list

val make_proof : Proposition.t list -> Proposition.t list -> t

val get_hyp : int -> t -> Proposition.t

val add_goal : Proposition.t -> t

val add_hyp : Proposition.t -> t

val get_first_goal : t -> Proposition.t

val is_proven : t -> bool

val remove_hyp : int -> t -> Proposition.t list

val clean : t -> t
