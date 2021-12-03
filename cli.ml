(* cli.ml interface*)

open Hypothese;;


let show_id_hypo h = Printf.printf "%d"  h.id;;



let prop_iter = fun c_n c_t c_f f_neg f_imply prop ->
let rec prop_to_string = fun p ->
match p with
  |Name n -> c_n n
  |True ->  c_t
  |False -> c_f 
  |Negation neg -> f_neg (prop_to_string neg) 
  |Implies (p1,p2) -> f_imply (prop_to_string  p1)  (prop_to_string  p2) in 
prop_to_string  prop;;

 (* On peut utiliser le module Uchar pour avoir les caractères unicode mathématiques*)


let c_name = fun n -> "name";;
let c_true = "True";;
let c_false = "False" ;;

let f_implies=fun s1 s2 -> String.concat "=>" [s1;s2];; 

let f_negation=fun s -> String.concat " " ["Not";s];;





