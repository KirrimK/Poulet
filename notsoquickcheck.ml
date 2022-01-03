(* Module NotSoQuickCheck *)

(* Générer une proposition aléatoire *)


(* Générer un but aléatoire *)


(* Générer un contexte aléatoire *)
let add_rand_cont = fun max_prop_depth hyp_quantity proof ->
  let rec it = fun cont acc->
    if cont > 0 then
      it (cont-1) (add_hyp (propAleatoire max_prop_depth) acc)
    else
      acc in
  let new_proof = it hyp_quantity proof in
  (true, proof);;

let get_rand_cont = fun max_prp_dp hp->
  add_rand_cont max_prp_dp hp empty_proof;;;

(* Générer un but garanti prouvable à partir d'un contexte *)


(* Tester les performances du backtrack *)
