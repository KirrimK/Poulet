(* cli.ml interface*)

open Strategies;;
open String;;

let show_id_hypo h = Printf.printf "%d"  h.id;;



let prop_iter = fun c_n c_t c_f f_neg f_imply f_and  prop ->
  let rec prop_to_string = fun p ->
    match p with
      |Name n -> c_n n
      |True  ->  c_t 
      |False  -> c_f 
      |Negation neg -> f_neg (prop_to_string neg) 
      |Implies (p1,p2) -> f_imply (prop_to_string  p1)  (prop_to_string  p2)  
      |And (p1,p2) -> f_and (prop_to_string p1) (prop_to_string p2) in
  prop_to_string  prop


 (* On peut utiliser le module Uchar pour avoir les caractï¿½res unicode mathï¿½matiques*)


let c_name = fun n -> n ;;
let c_true = "True";;
let c_false = "False" ;;

(*let and_symbol = Uchar.type  0x02C4;;*)

let f_implies=fun s1 s2 -> String.concat "" ["(";s1;"=>";s2;")"];; 

let f_negation=fun s -> String.concat "" ["Not";"(";s;")"];;

let f_and = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;"^";sProp2;")"];;

let show_prop = fun propo -> prop_iter c_name c_true c_false f_negation f_implies f_and  propo;;

let print_prop  propo =  Printf.printf "%s" (show_prop propo);;


