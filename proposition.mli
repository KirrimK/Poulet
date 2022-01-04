type t

val p_true : t

val p_false : t

val p_name : string -> t

val p_not : t -> t

val ( => ) : t -> t -> t

val ( ^ ) : t -> t -> t

val ( $ ) : t -> t -> t

exception Invalid_Input

val prop_iter : (string -> 'a) -> 'a -> 'a -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> t -> 'a

val polo_prop : string list -> t

val prop_root : t -> string

val prop_depth : t -> int