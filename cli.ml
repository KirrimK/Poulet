(* cli.ml interface*)

open Strategies;;
open String;;
open Backtrack;;

let show_id_hypo = fun hypo ->
  foncgen_hypo (Printf.printf "%d") (fun x -> ()) hypo;;

(* On peut utiliser le module Uchar pour avoir les caractères unicode mathématiques*)

let c_name = fun n -> n ;;
let c_true = "⊤";;
let c_false = "⊥" ;;

(*let and_symbol = Uchar.type  0x02C4;;*)

let f_implies = fun s1 s2 -> String.concat "" ["(";s1;"⇒";s2;")"];;

let f_and = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;" ∧ ";sProp2;")"];;

let f_or = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;" ∨ ";sProp2;")"];;

let prop_to_string = fun propo -> prop_iter c_name c_true c_false f_implies f_and f_or  propo;;

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

exception InvalidArgument

let traiter_cmde = fun str stateList shadd ->
  let split_str = string_to_list str in
  match split_str with
    ["back"] ->
      begin
        match !stateList with
          [] ->
            let () = shadd := false in
            let () = Printf.printf "Historique vide.\n" in
            (fun x -> (true, empty_proof))
        | smth::rest ->
            let () = shadd := false in
            let () = stateList := rest in
            (fun x -> (true, smth))
      end
  | ["intro"] -> intro
  | ["nettoyer"] -> (fun x -> (true, nettoyer x))
  | ["andSplit"] -> andsplit
  | ["assumption"] -> assumption
  | "andSplitHypo"::rest->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
            andSplitHypo hyp_num
        | _ -> raise InvalidArgument
      end
  | ["left"] -> orSplit false
  | ["right"] -> orSplit true
  | "add_hyp"::rest ->
        (fun x -> (true, add_hyp x (make_prop rest)))
  | "add_goal"::rest ->
        (fun x -> (true, add_remainder x (make_prop rest)))
  | "orSplitHypo-left"::rest ->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
            orSplitHypo false hyp_num
        | _ -> raise InvalidArgument
      end
  | "orSplitHypo-right"::rest ->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
            orSplitHypo true hyp_num
        | _ -> raise InvalidArgument
      end
  | "exact"::rest ->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
            exact hyp_num
        | _ -> raise InvalidArgument
      end
  | "apply"::rest ->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
            apply hyp_num
        | _ -> raise InvalidArgument
      end
  | _ -> (fun x -> (false, x));;

(* REPL: Read-Eval-Print Loop
   Prendre une entrée, la parser pour en sortir une commande,
   exécuter la commande sur la preuve,
   recommencer jusqu'à quitter
*)
(* Fonctionne à peu près dans l'état actuel, ne peut être quittée proprement
   Code assez moche (avec des pointeurs chelous), manque le backtrack *)
let repl = fun () ->
  (* Mettre ici des prints lors du lancement du programme *)
  let () = Printf.printf "Poulet v0.01 indev\nCopyright 2021 - 2022 Brévart, Courtadon, Dutau, de Crevoisier\n" in
  let proof = ref empty_proof in
  let stateList = ref [] in
  let should_add = ref true in
  Printf.printf "%s\n" (proof_to_string !proof);
  let finished = false in
  while not finished do
    Printf.printf "> ";
    (* Récupérer l'entrée utilisateur *)
    let input = read_line () in
    (* Comprendre quelle commande à utiliser *)
    let () =
      try
        let cmde = traiter_cmde input stateList should_add in
        (* Evaluer la commande et mettre à jour l'état de preuve si aucune exception levée et commande réussie *)
        let () =
          try
            let (res, new_proof) = cmde !proof in
            if res then
              begin
                if !should_add then
                  let () = stateList := (!proof)::(!stateList) in
                  proof := new_proof;
                else
                  let () = should_add := true in
                  proof := new_proof;
              end
            else
              Printf.printf "La commande a échoué.\n"
          with _ ->
            Printf.printf "Erreur lors de l'exécution de la commande.\n"
        in
        ()
      with _ ->
        Printf.printf "Commande incorrecte.\n" in
    Printf.printf "%s\n" (proof_to_string !proof);
  done;;

