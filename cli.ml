(* cli.ml interface*)

open Proposition;;
open Strategies;;
open Proof;;
(*open String;; unused d'après Dune*)
open Backtrack;;
open Notsoquickcheck;;

(*let show_id_hypo = fun hypo ->
  foncgen_hypo (Printf.printf "%d") (fun x -> ()) hypo;;
*)
(* On peut utiliser le module Uchar pour avoir les caractères unicode mathématiques*)

let version_code = "0.1";;

let c_name = fun n -> n ;;
let c_true = "⊤";;
let c_false = "⊥" ;;

(*let and_symbol = Uchar.type  0x02C4;;*)

let f_implies = fun s1 s2 -> String.concat "" ["(";s1;" ⇒ ";s2;")"];;

let f_and = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;" ∧ ";sProp2;")"];;

let f_or = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;" ∨ ";sProp2;")"];;

let prop_to_string = fun propo -> prop_iter c_name c_true c_false f_implies f_and f_or propo;;

let print_prop = fun propo -> Printf.printf "%s" (prop_to_string propo);;

let string_to_list = fun str -> String.split_on_char ' ' str;;

let hyp_to_string = fun id prop ->
  String.concat "" [string_of_int id; "\t: "; prop_to_string prop];;

let proof_to_string = fun proof->
  (* Afficher les hypothèses *)
  let hypStrings = List.map2 (fun x y -> hyp_to_string x y) (hyp_ids proof) (get_hyps proof) in
  let hypsString = String.concat "\n" hypStrings in
  let goalStrings = List.map (prop_to_string) (get_goal proof) in
  let goalString = String.concat "\n" goalStrings in
  String.concat "\n" [""; hypsString; String.make 15 '-'; goalString];;

exception InvalidArgument

let hpf_cli = fun id proof ->
  prop_to_string (get_hyp id proof);;

let print_help = fun () ->
  Printf.printf "  Poulet v%s: REPL Help" version_code;
  Printf.printf "
  The current state of the proof is displayed like this:
  (ids    : hypotheses)
  ---------------
  (goals)

  Formulas are currently typed in stack-based reverse form:
  ex: \"add_hyp A B =>\" gives \"id: (A ⇒ B)\"
      \"add_goal A B C => =>\" gives \"(A ⇒ (B ⇒ C))\"

  Formula operators are:
             \"=>\" : implies
              \"^\" : and
              \"v\" : or
            \"Not\" : not
           \"True\" : ⊤ (literal true)
          \"False\" : ⊥ (literal false)
  (anything else) : proposition name

  List of available commands:
  - help: displays this help
  - back: reverts to the previous state
  - q: quits this program
  - clean: reorders the hypotheses and goals and deletes duplicated items in the current proof state
  - add_hyp <formula>
  - add_goal <formula>
  - add_random_goal <max depth>
  - add_random_context <hyp max depth> <hyp number>

  List of available proof strategies:
  - intro
  - split
  - isfalse <hyp id>
  - hyp_split <hyp id>
  - left
  - right
  - hyp_left <hyp id>
  - hyp_right <hyp id>
  - apply <hyp id>
  - applyin <hyp id:to modify> <hyp id:to apply> <\"keep\": optionnal>
  - exact <hyp id>
  - assumption
  - auto <\"verbose\": optionnal>\n";;

let traiter_cmde = fun str stateList shadd fin ->
  let split_str = string_to_list str in
  match split_str with
    ["q"] ->
      fin := true;
      (fun x -> (true, x))
  | ["help"] ->
      let () = shadd := false in
      print_help ();
      (fun x -> (true, x))
  | ["back"] ->
      begin
        match !stateList with
          [] ->
            let () = shadd := false in
            let () = Printf.printf "Historique vide.\n" in
            (fun x -> (true, Proof.empty))
        | smth::rest ->
            let () = shadd := false in
            let () = stateList := rest in
            (fun x -> (true, smth))
      end
  | ["intro"] -> intro
  | ["clean"] -> (fun x -> (true, clean x))
  | ["split"] -> split
  | ["assumption"] -> assumption
  | "auto"::rest ->
      begin
        match rest with
          ["verbose"] -> (fun x -> backtrack x true hpf_cli)
        | _ -> (fun x -> backtrack x false hpf_cli)
      end
  | "hyp_split"::rest->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
            hyp_split hyp_num
        | _ -> raise InvalidArgument
      end
  | ["left"] -> left
  | ["right"] -> right
  | "add_hyp"::rest ->
        (fun x -> (true, add_hyp (polo_prop rest) x))
  | "add_goal"::rest ->
        (fun x -> (true, add_goal (polo_prop rest) x))
  | "hyp_left"::rest ->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
            hyp_left hyp_num
        | _ -> raise InvalidArgument
      end
  | "hyp_right"::rest ->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
            hyp_right hyp_num
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
 | "applyin"::rest ->
      begin
        match rest with
          arga::resta ->
            let hyp_numa = int_of_string arga in
            begin
              match resta with
                [argb] ->
                  let hyp_numb = int_of_string argb in
                  applyInHyp false hyp_numa hyp_numb
              | argb::["keep"] ->
                  let hyp_numb = int_of_string argb in
                  applyInHyp true hyp_numa hyp_numb
              | _ -> raise InvalidArgument
            end
        | _ -> raise InvalidArgument
      end
  | "isfalse"::rest ->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
            false_hyp hyp_num
        | _ -> raise InvalidArgument
      end
  | "add_random_goal"::rest ->
      begin
        match rest with
          [arg] ->
            let num_arg = int_of_string arg in
            add_rand_goal num_arg
        | _ -> raise InvalidArgument
      end
  | "add_random_context"::rest ->
      begin
        match rest with
          arga::resta ->
            let hyp_numa = int_of_string arga in
            begin
              match resta with
                [argb] ->
                  let hyp_numb = int_of_string argb in
                  add_rand_cont hyp_numa hyp_numb
              | _ -> raise InvalidArgument
            end
        | _ -> raise InvalidArgument
      end
  | _ -> raise InvalidArgument;;

(* REPL: Read-Eval-Print Loop
   Prendre une entrée, la parser pour en sortir une commande,
   exécuter la commande sur la preuve,
   recommencer jusqu'à quitter
*)
(* Fonctionne à peu près dans l'état actuel, ne peut être quittée proprement
   Code assez moche (avec des pointeurs chelous), manque le test de false dans les hypotheses *)
let repl = fun () ->
  (* Mettre ici des prints lors du lancement du programme *)
  let () = Printf.printf "Poulet v%s\nCopyright 2021 - 2022 Brévart, Courtadon, Dutau, de Crevoisier\nType \"help\" for help.\n" version_code in
  let proof = ref Proof.empty in
  let stateList = ref [] in
  let should_add = ref true in
  Printf.printf "%s\n" (proof_to_string !proof);
  let finished = ref false in
  while not !finished do
    Printf.printf "> ";
    (* Récupérer l'entrée utilisateur *)
    let input = read_line () in
    (* Comprendre quelle commande à utiliser *)
    let () =
      try
        let cmde = traiter_cmde input stateList should_add finished in
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
          with e ->
            Printf.printf "%s\n" (Printexc.to_string e);
            Printf.printf "Erreur lors de l'exécution de la commande.\n"
        in
        ()
      with _ ->
        Printf.printf "Commande incorrecte.\n" in
    if not !finished then Printf.printf "%s\n" (proof_to_string !proof);
  done;;
