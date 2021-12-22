(* cli.ml interface*)

open Strategies;;
open String;;

let show_id_hypo = fun hypo ->
  foncgen_hypo (Printf.printf "%d") (fun x -> ()) hypo;;

(* On peut utiliser le module Uchar pour avoir les caractères unicode mathématiques*)

let c_name = fun n -> n ;;
let c_true = "⊤";;
let c_false = "⊥" ;;

(*let and_symbol = Uchar.type  0x02C4;;*)

let f_implies = fun s1 s2 -> String.concat "" ["(";s1;"⇒";s2;")"];; 

let f_negation = fun s -> String.concat "" ["¬";"(";s;")"];;

let f_and = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;" ∧ ";sProp2;")"];;

let f_or = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;" ∨ ";sProp2;")"];;

let prop_to_string = fun propo -> prop_iter c_name c_true c_false f_negation f_implies f_and f_or  propo;;

let print_prop = fun propo -> Printf.printf "%s" (prop_to_string propo);;

let string_to_list = fun str -> String.split_on_char ' ' str;;

let hyp_to_string = fun hypo ->
  let (strid, strprop) = foncgen_hypo (fun x -> string_of_int x) (fun x -> prop_to_string x) hypo in
  String.concat ": "  [strid; strprop];;

let proof_to_string = fun proof->
  (* Afficher les hypothèses *)
  let hypStrings = List.map (fun x -> hyp_to_string x) (getHypList proof) in
  let hypsString = String.concat "\n" hypStrings in
  let remainderStrings = List.map (prop_to_string) (getRemainder proof) in
  let remainderString = String.concat "\n" remainderStrings in
  String.concat "\n" [hypsString; "-----"; remainderString];;

(* idées pour l'interface: si une commande à une nature à splitter le pb, proposer de switcher de problème avec "switch <nb probl>", et indiquer sur la ligne avant l'entrée utilisateur les infos ex.

0: A => B
1: C
---
C => B
/1\ [2]
> switch 2
0: A => B
1: C
---
D => A
[1] /2\
>
*)
