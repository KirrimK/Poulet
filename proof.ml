(* Proof.ml *)

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
      it (nb-1) nb::acc
    else
      acc in
  it (List.length (get_hyps proof)) [];;

let make_proof = fun hyps_ goal_->
   {hyps=hyps_; goal=goal_};;

let get_hyp = fun id proof ->
  let rec it = fun
