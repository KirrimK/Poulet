type t

val p_true : t

val p_false : t

val p_name : string -> t

val p_not : t -> t

val ( => ) : t -> t -> t

val ( ^ ) : t -> t -> t

val ( $ ) : t -> t -> t

exception Invalid_Input

val prop_match : (string -> 'a) -> 'a -> 'a -> (t -> t -> 'a) -> (t -> t -> 'a) -> (t -> t -> 'a) -> t -> 'a

val prop_iter : (string -> 'a) -> 'a -> 'a -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> ('a -> 'a -> 'a) -> t -> 'a

val p_matchname : (string -> 'a) -> 'a -> t -> 'a

val p_matchtrue : 'a -> 'a -> t -> 'a

val p_matchfalse : 'a -> 'a -> t -> 'a

val p_matchimpl : (t -> t -> 'a) -> 'a -> t -> 'a

val p_matchand : (t -> t -> 'a) -> 'a -> t -> 'a

val p_matchor : (t -> t -> 'a) -> 'a -> t -> 'a

val polo_prop : string list -> t

val prop_root : t -> string

val prop_depth : t -> int

val prop_items : t -> int
