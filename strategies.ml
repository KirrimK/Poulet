(* Strategies.ml *)

open Hypothese;;

let intro = fun proo ->
  let nexthypname = (hd proo.hypos).id + 1 in
  match hd proo.remainder with
    Implies(True, _) -> (false, proo)
  | Implies(False, _) -> (false, proo)
  | Implies(a, b) -> (true, {hypos={name=; prop=a}::proo.hypos; 
  | _ -> (false, proo);;
