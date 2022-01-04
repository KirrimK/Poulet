(* Fichier définissant le type hypothèse. *)

type t = {
    id: int;
    prop: proposition;
  };;

let getProp = fun hypo -> hypo.prop;;

let getId = fun hypo -> hypo.id;;

let newHypo = fun idH propo -> {id=idH; prop= propos};;