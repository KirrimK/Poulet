type t

val empty : t

val get_hyps : t -> Proposition.t list

val get_goal : t -> Proposition.t list

val hyp_ids : t -> int list

val make_proof : Proposition.t list -> Proposition.t list -> t

val get_hyp : int -> t -> Proposition.t

