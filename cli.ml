(* cli.ml interface*)

open Hypothese;;


let show_hypo h = Printf.printf "%s :  %d" h.name h.id

let rec prop_to_string = fun p ->
match p with
  |Name n -> n   
  |True t-> "%s True" t
  |False f->"%s False" f
  |Implies (p1,p2) -> prop_to_string p1 + "=>" + prop_to_string p2 ;;

 (* On peut utiliser le module Uchar pour avoir les caractères unicode mathématiques*)


let show_prop p =  Printf.printf "%s" prop_to_string p



