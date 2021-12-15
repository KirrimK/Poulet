(* cli.ml interface*)

open Strategies;;
open String;;

let show_id_hypo = fun hypo ->
  foncgen_hypo (Printf.printf "%d") (fun x -> ()) hypo;;

(* On peut utiliser le module Uchar pour avoir les caractères unicode mathématiques*)

let c_name = fun n -> n ;;
let c_true = "True";;
let c_false = "False" ;;

(*let and_symbol = Uchar.type  0x02C4;;*)

let f_implies = fun s1 s2 -> String.concat "" ["(";s1;"=>";s2;")"];; 

let f_negation = fun s -> String.concat "" ["Not";"(";s;")"];;

let f_and = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;"^";sProp2;")"];;

let show_prop = fun propo -> prop_iter c_name c_true c_false f_negation f_implies f_and  propo;;

let print_prop = fun propo -> Printf.printf "%s" (show_prop propo);;
