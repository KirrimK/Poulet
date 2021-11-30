(* cli.ml interface*)

open Hypothese;;


let show_id_hypo h = Printf.printf "%d"  h.id;;


let prop_iter = fun c_n c_t c_f f_neg f_imply prop ->
let rec prop_to_string = fun p ->
match p with
  |Name n -> c_n
  |True ->  c_t
  |False -> c_f 
  |Negation neg -> f_neg neg 
  |Implies (p1,p2) -> f_imply (prop_to_string  p1)  (prop_to_string  p2) in 
prop_to_string  prop;;





 (* On peut utiliser le module Uchar pour avoir les caractères unicode mathématiques*)


(*let show_prop = fun p ->  Printf.printf "%s" prop_iter  p;; *)



