(* cli.mli *)

val prop_to_string: Proposition.t -> string;;

val string_to_list: string -> string list;;

val proof_to_string: Proof.t -> string;;

val repl: unit -> unit;;


