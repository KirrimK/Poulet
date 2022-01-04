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

let hyp_ids = fun proof ->
  let rec it = fun nb acc->
    if nb > 0 then
      it (nb-1) (nb::acc)
    else
      acc in
  it (List.length (get_hyps proof)) [];;

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
    | _ -> failwith "l'id n'existe pas dans la liste" in
  it 0 (get_hyps proof);;

let get_first_goal = fun proof ->
  List.hd (get_goal proof);;

let is_proven = fun proof ->
  ((get_goal proof) = []);;

let remove_hyp = fun id proof ->
  let rec it = fun nb acc acc_ok->
    match acc with
      a::rest ->
        if id = nb then
          it (nb+1) rest acc_ok
        else
          it (nb+1) rest (a::acc_ok)
    | _ -> failwith "l'id n'existe pas dans la liste" in
  it 0 (get_hyps proof);;

let clean = fun proof ->
  make_proof (List.sort_uniq compare (List.filter (fun x -> x <> p_true) (get_hyps proof)) (List.sort_uniq compare (List.filter (fun x -> x <> p_true) (get_goal proof));;
