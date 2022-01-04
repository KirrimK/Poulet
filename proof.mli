type t

val empty : t

val get_hyps : t -> Hypothese.t list

val get_goal : t -> Proposition.t list

val hyp_ids : t -> int list

val make_proof : Hypothese.t list -> Proposition.t list -> t

val getHyp : int -> t -> Hypothese.t

