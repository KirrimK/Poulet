(* cli.mli *)

val c_name : string -> string;;

val c_true: string;;

val c_false: string;;

val f_implies: string -> string -> string;;

val f_and: string -> string -> string;;

val f_or: string -> string -> string;;

val prop_to_string: Proposition.t -> string;;

val print_prop:  Proposition.t -> unit;;

val string_to_list: string -> string list;;

val proof_to_string: Strategies.proof -> string;;

val repl: unit -> unit;;
