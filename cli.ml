
(* cli.ml interface*)

open Proposition;;
open Strategies;;
open Proof;;
(*open String;; unused d'après Dune*)
open Backtrack;;
open Random_props;;
open Tests;;
open Fileio;;

(*let show_id_hypo = fun hypo ->
  foncgen_hypo (Printf.printf "%d") (fun x -> ()) hypo;;
*)
(* On peut utiliser le module Uchar pour avoir les caractères unicode mathématiques*)

let version_code = "1.0";;

let c_name = fun n -> n ;;
let c_true = "⊤";;
let c_false = "⊥" ;;

(*let and_symbol = Uchar.type  0x02C4;;*)

let f_implies = fun s1 s2 -> String.concat "" ["(";s1;" ⇒ ";s2;")"];;

let f_and = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;" ∧ ";sProp2;")"];;

let f_or = fun sProp1 sProp2 -> String.concat "" ["(";sProp1;" ∨ ";sProp2;")"];;

let prop_to_string = fun propo ->
  if prop_items propo > 100 then
    String.concat "" ["(proposition of depth "; string_of_int (prop_depth propo); " and "; string_of_int (prop_items propo) ; " items | root is "; prop_root propo; ")"]
  else
    prop_iter c_name c_true c_false f_implies f_and f_or propo;;

let print_prop = fun propo -> Printf.printf "%s" (prop_to_string propo);;

let string_to_list = fun str -> String.split_on_char ' '  str;;

let hyp_to_string = fun id prop ->
  String.concat "" [string_of_int id; "\t: "; prop_to_string prop];;

let proof_to_string = fun proof->
  (* Afficher les hypothèses *)
  let hypStrings = List.mapi (fun x y -> hyp_to_string x y) (get_hyps proof) in
  let hypsString = String.concat "\n" hypStrings in
  let goalStrings = List.mapi (fun x y -> hyp_to_string x y)(get_goal proof) in
  let goalString = String.concat "\n" goalStrings in
  String.concat "\n" [""; hypsString; String.make 15 '-'; goalString];;

exception InvalidArgument

let print_help = fun () ->
  Printf.printf "  Poulet v%s: REPL Help" version_code;
  Printf.printf "
  The current state of the proof is displayed like this:
  (ids    : hypotheses)
  ---------------
  (goals)

  Formulas must be typed  typed in this form:
  ex: \"add_hyp A=>B\" gives \"id: (A ⇒ B)\"
      \"add_goal A=>B=>C \" gives \"(A ⇒ (B ⇒ C))\"

  Formula operators are:
             \"=>\" : implies
              \"^\" : and
              \"|\" : or
            \"Not\" : not
           \"TRUE\" : ⊤ (literal true)
          \"FALSE\" : ⊥ (literal false)
  (anything else) : proposition name

  List of available commands:
  - help: displays this help
  - back: reverts to the previous state
  - empty: reset the proof to zero
  - q: quits this program
  - clean: reorders the hypotheses and goals and deletes duplicated items in the current proof state
  - add_hyp <formula>
  - add_goal <formula>
  - add_random_goal <max_depth>
  - add_random_context <hyp max_depth> <hyp_number>
  - get_random_context <hyp max_depth> <hyp_number>
  - reverse
  - unittests

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
            (fun _ -> (true, Proof.empty))
        | smth::rest ->
            let () = shadd := false in
            let () = stateList := rest in
            (fun _ -> (true, smth))
      end
  | ["intro"] -> intro
  | ["clean"] -> (fun x -> (true, clean x))
  | ["split"] -> split
  | ["assumption"] -> assumption
  (*| ["reverse"] -> reverse*)
  (*| ["get_provable"] -> (fun _ -> get_provable ())*)
  | ["empty"] -> let () = shadd := false in
    (fun _ -> (true, Proof.empty))
  | ["unittests"] -> let () = shadd := false in
    let () = tests () in
    (fun x -> (true, x))
  (*| "backtests"::rest ->
       begin
        match rest with
          [arg] ->
            let num_arg = int_of_string arg in
            let () = reverse_provable_test num_arg in
            (fun x -> (true, x))
        | _ -> raise InvalidArgument
      end*)
  | "auto"::rest ->
      begin
        match rest with
          ["verbose"] -> backtrack 2 prop_to_string
        | _ ->  backtrack 1 prop_to_string
      end
  | "hyp_split"::rest->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
            hyp_split hyp_num
        | _ -> raise InvalidArgument
      end
  | "select_goal"::rest->
      begin
        match rest with
          [arg] ->
            let hyp_num = int_of_string arg in
             select_goal hyp_num
        | _ -> raise InvalidArgument
      end
  | ["left"] -> left
  | ["right"] -> right
  | "add_hyp"::rest ->
    let formula = String.concat " " rest in
    let formula_bis = String.concat "" [formula;"\n"] in
    let lexbuf = Lexing.from_string formula_bis in
    let propo = Parser.main Lexer.token lexbuf in
    (fun x -> (true, add_hyp propo  x))
  | "add_goal"::rest ->
    let formula = String.concat " " rest in
    let formula_bis=String.concat "" [formula;"\n"] in
    let lexbuf = Lexing.from_string formula_bis in
    let propo = Parser.main Lexer.token lexbuf in
    (fun x -> (true, add_goal propo  x))
  | "load"::rest ->
    let filename = String.concat " " rest in
    (fun _ -> (true, load_from_file filename))
   | "save"::rest ->
    let filename = String.concat " " rest in
    (fun x -> let () = writeInFile filename x in (true, x))
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
   Prendre une entrée, la parser pour en sortir une commande avec des arguments (ou pas),
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
      with e ->
        Printf.printf "%s\n" (Printexc.to_string e);
        Printf.printf "Commande incorrecte.\n" in
    if not !finished then Printf.printf "%s\n" (proof_to_string !proof);
  done;;
