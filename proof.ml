(* Proof.ml *)

open Proposition;;

type t = {
    hyps: Proposition.t list;
    goal: Proposition.t list
};;

let empty = {hyps=[]; goal=[]};;

let get_hyps = fun proof ->
  proof.hyps;;

let get_goal = fun proof ->
  proof.goal;;

let list_ids = fun list ->
  let rec it = fun nb acc->
    if nb >= 0 then
      it (nb-1) (nb::acc)
    else
      acc in
  it (List.length (list) -1) [];;

let hyp_ids = fun proof ->
  list_ids (get_hyps proof);;

let goal_ids = fun proof ->
  list_ids (get_goal proof);;

let make_proof = fun hyps_ goal_->
   {hyps=hyps_; goal=goal_};;

let add_hyp = fun prop proof->
  make_proof (prop::(get_hyps proof)) (get_goal proof);;

let add_goal = fun prop proof->
  make_proof (get_hyps proof) (prop::(get_goal proof));;

let get_hyp = fun id proof ->
  let rec it = fun nb acc->
    match acc with
      a::rest ->
        if id = nb then
          a
        else
          it (nb+1) rest
    | _ -> failwith (String.concat "" ["L'id "; string_of_int id; " n'existe pas dans la liste"]) in
  it 0 (get_hyps proof);;

let get_first_goal = fun proof ->
  List.hd (get_goal proof);;

let is_proven = fun proof ->
  ((get_goal proof) = []);;

let remove_item_list = fun id ls ->
  let rec it = fun nb acc acc_ok->
    match acc with
      a::rest ->
        if id = nb then
          it (nb+1) rest acc_ok
        else
          it (nb+1) rest (a::acc_ok)
    | _ -> acc_ok in
  it 0 ls [];;

let remove_hyp = fun id proof ->
  remove_item_list id (get_hyps proof);;

let rm_duplic = fun list->
  let rec rm_rec = fun ls acc->
    match ls with
      a::rest ->
        if List.mem a acc then
          rm_rec rest acc
        else
          rm_rec rest (a::acc)
    | [] -> List.rev acc in
  rm_rec list [];;

(* Enleves les duplications, les vrais, et trie les elts (sauf le premier but (le but actif) qui reste en premier *)
let clean = fun proof ->
  let goals_no_duplic = (List.filter (fun x -> x <> p_true) (rm_duplic (get_goal proof))) in
  let goal_first_and_tail_sorted =
    (match goals_no_duplic with
      first::rest -> first::(List.sort compare rest)
    | [] -> []) in
  make_proof (List.sort_uniq compare (List.filter (fun x -> x <> p_true) (get_hyps proof))) goal_first_and_tail_sorted;;

let proof_goal_depth = fun proof ->
  (List.fold_left max 0 (List.map prop_depth (get_goal proof)))+(List.length (get_goal proof))-1;;

let proof_goal_items = fun proof ->
  List.fold_left (+) 0 (List.map prop_items (get_goal proof));;
