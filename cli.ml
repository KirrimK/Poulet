(* cli.ml interface*)

open Proposition;;
open Strategies;;
open Proof;;
open Backtrack;;
open Random_props;;
open Tests;;
open Fileio;;

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
            \"NOT\" : not
           \"TRUE\" : ⊤ (literal true)
          \"FALSE\" : ⊥ (literal false)
    \"(\" and \")\" : parenthesis
    (anything else) : proposition name

  List of available commands:
  - add_goal <formula>
  - add_hyp <formula>
  - add_random_context <hyp max_depth> <hyp_number>  
  - add_random_goal <max_depth>
  - back: reverts to the previous state
  - clean: reorders the hypotheses and goals and deletes duplicated items in the current proof state
  - empty: reset the proof to zero
  - help: displays this help
  - q: quits this program
  - remove_goal <goal_number>
  - select_goal <goal_number>
  - unittests : runs tests
  - chicken


  List of available proof strategies:
  - apply <hyp id>
  - applyin <hyp id:to modify> <hyp id:to apply> <\"keep\": optionnal>
  - assumption
  - auto <\"verbose\": optionnal>
  - exact <hyp id>
  - hyp_split <hyp id>
  - hyp_left <hyp id>
  - hyp_right <hyp id>
  - intro
  - isfalse <hyp id>
  - left
  - right
  - split\n";;

type cli_state = {
    proof: Proof.t;
    history: Proof.t list;
    has_ended: bool
};;

(* REPL: Read-Eval-Print Loop
   Prendre une entrée, la parser pour en sortir une commande avec des arguments (ou pas),
   exécuter la commande sur la preuve,
   recommencer jusqu'à quitter
*)

let traiter_input = fun str state->

  let previous_in_history = state.proof::state.history in

  let str_list = string_to_list str in

  let one_arg = fun yesarg noarg ls->
    try
        match ls with
          a::_ -> yesarg a
        | [] -> noarg ()
    with e ->
        let () = Printf.printf "%s\n" (Printexc.to_string e) in
        state in

  let two_and_optionnal = fun yesargopt nope ls->
    try
        match ls with
        arga::resta ->
            begin
            match resta with
            | argb::tl -> yesargopt arga argb tl
            | _ -> nope ()
            end
        | _ -> nope ()
    with e ->
        let () = Printf.printf "%s\n" (Printexc.to_string e) in
        state in

  let apply_strat = fun strat->
    let (res, resproof) = strat state.proof in
    if res then
      {proof=resproof; history=previous_in_history; has_ended=false}
    else
      let () = Printf.printf "Strategy has failed.\n" in
      state in

  let missing_arg = (fun () -> let () = Printf.printf "Missing an argument.\n" in state) in

  let one_arg_strat = fun strat rest ->
    one_arg
      (fun x -> let hyp_num = int_of_string x in apply_strat (strat hyp_num)) missing_arg rest in

  let try_parse = fun goal rest ->
    try
        let formula = String.concat " " rest in
        let formula_bis = String.concat "" [formula;"\n"] in
        let lexbuf = Lexing.from_string formula_bis in
        let propo = Parser.main Lexer.token lexbuf in
        {proof=(if goal then add_goal else add_hyp) propo state.proof; history=previous_in_history; has_ended=false}
    with e ->
        let () = Printf.printf "%s\n" (Printexc.to_string e) in state in

  match str_list with
    ["q"] ->
    {proof=state.proof; history=state.history; has_ended=true}
  | ["chicken"] -> let () = Printf.printf "Here's a chicken to keep you company:\n   MM\n  <' \\___/|\n    \\_  _/\n      ][\n" in state
  | ["help"] ->
    let () = print_help () in
    {proof=state.proof; history=state.history; has_ended=true}
  | ["back"] ->
    begin
      match state.history with
        a::rest -> {proof=a; history=rest; has_ended=false}
        | [] -> let () = Printf.printf "History is empty.\n" in state
    end
  | ["clean"] ->
    {proof=clean state.proof; history=previous_in_history; has_ended=false}
  | ["intro"] ->
    apply_strat intro
  | ["split"] ->
    apply_strat split
  | ["assumption"] ->
    apply_strat assumption
  | ["empty"] ->
    {proof=Proof.empty; history=previous_in_history; has_ended=false}
  | ["unittests"] ->
    let () = tests () in
    state
  | "auto"::rest ->
    one_arg (fun x ->
        if x = "verbose" then
          apply_strat (backtrack 2 prop_to_string)
        else
          let () = Printf.printf "Unknown argument.\n" in
          state) (fun () -> apply_strat (backtrack 1 prop_to_string)) rest
  | ["left"] -> apply_strat left
  | ["right"] -> apply_strat right
  | "hyp_split"::rest ->
    one_arg_strat hyp_split rest
  | "select_goal"::rest ->
    one_arg_strat select_goal rest
  | "hyp_left"::rest ->
    one_arg_strat hyp_left rest
  | "hyp_right"::rest ->
    one_arg_strat hyp_right rest
  | "exact"::rest ->
    one_arg_strat exact rest
  | "apply"::rest ->
    one_arg_strat apply rest
  | "isfalse"::rest ->
    one_arg_strat false_hyp rest
  | "remove_goal"::rest ->
    one_arg (fun x-> let new_proof = remove_item_list (int_of_string x) state.proof in {proof=new_proof; history=previous_in_history; has_ended=false}) missing_arg rest
  | "applyin"::rest ->
    two_and_optionnal
        (fun x y z ->
            let hyp_numa = int_of_string x in
            let hyp_numb = int_of_string y in
            apply_strat (applyInHyp (if z = ["keep"] then true else false) hyp_numa hyp_numb))
        missing_arg rest
  | "add_hyp"::rest ->
    try_parse false rest
  | "add_goal"::rest ->
    try_parse true rest
  | "load"::rest ->
    begin
    try
        let filename = String.concat " " rest in
        {proof=load_from_file filename; history=previous_in_history; has_ended=false}
    with e ->
        let () = Printf.printf "%s\n" (Printexc.to_string e) in state
    end
  | "save"::rest ->
    begin
    try
        let filename = String.concat " " rest in
        let () = writeInFile filename state.proof in
        state
    with e ->
        let () = Printf.printf "%s\n" (Printexc.to_string e) in state
    end
  | "add_random_goal"::rest ->
    one_arg_strat add_rand_goal rest
  | "add_random_context"::rest ->
    two_and_optionnal (fun x y _->
        let num_arga = int_of_string x in
        let num_argb = int_of_string y in
        let (_, new_proof) = add_rand_cont num_arga num_argb state.proof in {proof=new_proof; history=previous_in_history; has_ended=false}) missing_arg rest
  | _ ->
    let () = Printf.printf "Unknown command.\n" in 
    state;;

let repl = fun ()->
  let () = Printf.printf "Poulet v%s\nCopyright 2021 - 2022 Brévart, Courtadon, Dutau, de Crevoisier\nType \"help\" for help.\n" version_code in
  let rec repl_it = fun state ->
    if state.has_ended then
      () (* quit *)
    else
      let () = Printf.printf "%s\n> " (proof_to_string state.proof) in (*print proof *)
      let input = read_line () in (*get line from user*)
      repl_it (traiter_input input state) in (*treat input *)
  repl_it {proof=Proof.empty; history=[]; has_ended=false};;
