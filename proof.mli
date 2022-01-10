type a

type t = a list

val empty : t

val get_hyps : t -> Proposition.t list

val get_goal : t -> Proposition.t list

val hyp_ids : t -> int list

val goal_ids : t -> int list

val make_proof : Proposition.t list list -> Proposition.t list -> t

val make_a : Proposition.t list -> Proposition.t -> a

val get_hyp : int -> t -> Proposition.t

val add_goal : Proposition.t -> t -> t

val add_hyp : Proposition.t -> t -> t

val get_first_goal : t -> Proposition.t

val change_first_goal : Proposition.t -> t -> t

val is_proven : t -> bool

val remove_hyp : int -> t -> Proposition.t list

val remove_item_list : int -> 'a list -> 'a list

val place_elt_at_head : int -> 'a list -> 'a list

val rm_duplic : 'a list -> 'a list

val clean : t -> t

val proof_goal_depth : t -> int

val proof_goal_items : t -> int
