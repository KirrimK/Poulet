(* Proof.ml *)

open Proposition;;

type a = {
    hyps: Proposition.t list;
    goal: Proposition.t
};;

type t = a list;;

let empty = [];;

let get_hyps = fun proof ->
  match proof with
    hd::_ -> hd.hyps
  | [] -> [] ;;

let get_goal = fun proof ->
  List.map (fun x -> x.goal) proof;;

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

let make_proof = fun hyp_ls_ls goal_ls->
  List.map2 (fun x y-> {hyps = x; goal = y}) hyp_ls_ls goal_ls;; 

let demake_proof = fun proof->
  (List.map (fun x->x.hyps) proof, List.map (fun x -> x.goal) proof);; 
  
let make_a = fun hyp_ls goal->
  {hyps=hyp_ls; goal=goal};;

let add_hyp_to_a = fun hyp ancienA -> make_a (hyp::ancienA.hyps) ancienA.goal;;
  
let add_hyp = fun prop proof->
  match proof with
    hd::rest ->
      make_proof ((prop::hd.hyps)::(List.map (fun x->x.hyps) rest)) (get_goal proof)
  | [] -> failwith "Add a goal first."

let add_goal = fun prop proof->
  {hyps=[]; goal=prop}::proof;;

let change_first_goal = fun prop proof->
  match proof with
    hd::rest -> {hyps=hd.hyps; goal=prop}::rest
  | [] -> failwith "Add a goal first." ;;
  
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

let place_elt_at_head = fun id list->
  let rec get_elt = fun ls count->
    match ls with
      a::rest ->
        if id = count then
          a
        else
          get_elt rest (count+1)
    | _ -> failwith (Printf.sprintf "id %d doesn't exist in list.\n" id) in
  let elt = get_elt list 0 in
  elt::(remove_item_list id list);;

let rm_duplic_ = fun crit list->
  let rec rm_rec = fun ls memacc outacc->
    match ls with
      a::rest ->
        if List.mem (crit a) memacc then
          rm_rec rest memacc outacc
        else
          rm_rec rest ((crit a)::memacc) (a::outacc)
    | [] -> List.rev outacc in
  rm_rec list [] [];;

let rm_duplic = fun list->
  rm_duplic_ (fun x->x) list;;
  
let clean = fun proof ->
  let goals_no_true = (List.filter (fun x -> x.goal <> p_true) proof) in
  let goals_no_duplic = (rm_duplic_ (fun x-> x.goal) goals_no_true) in
  let goals_others_sorted = 
    match goals_no_duplic with
      hd::rest -> hd::(List.sort (fun x y->compare x.goal y.goal) rest)
    | [] -> [] in
  List.map (fun x -> {hyps=(List.sort_uniq compare (List.filter (fun x -> x <> p_true) x.hyps)); goal=x.goal}) goals_others_sorted;;

let proof_goal_depth = fun proof ->
  (List.fold_left max 0 (List.map prop_depth (get_goal proof)))+(List.length (get_goal proof))-1;;

let proof_goal_items = fun proof ->
  List.fold_left (+) 0 (List.map prop_items (get_goal proof));;
