(* Module NotSoQuickCheck *)
open Strategies;;
(* Générer une proposition aléatoire *)
(* propAleatoire : int -> prop *)

exception Invalid_Input;;

let propAleatoire = fun profondeurMax->
  (* Fonction qui génère une proposition de profondeur maximale déterminée. *)
  let idLibre = ref 0 in
  if profondeurMax < 1 then raise Invalid_Input
  else
  let rec iterateurLocal = fun profondeurMax ->
    let rFloat = Random.float 1.0 in
    if profondeurMax = 1
      then if rFloat < 0.4
        then begin incr idLibre ; p_name (Printf.sprintf ("P_%d") !idLibre) end
        else if rFloat < 0.8
          then p_name (Printf.sprintf ("P_%d") (Random.int (!idLibre + 1)))
          else if rFloat < 0.9
            then p_true
            else p_false
      else
        if rFloat < 0.04
          then begin incr idLibre ; p_name (Printf.sprintf "P_%d" !idLibre) end
          else if rFloat < 0.08
            then p_name (Printf.sprintf ("P_%d") (Random.int (!idLibre + 1)))
            else if rFloat<0.09
              then p_true
              else if rFloat <0.1
                then p_false
                else
                  let p1 = iterateurLocal (profondeurMax-1) in
                  let p2 = iterateurLocal (profondeurMax-1) in
                  if rFloat<0.4
                  then p1 => p2
                  else if rFloat < 0.7
                      then p1 ^ p2
                      else p1 $ p2 in
  iterateurLocal profondeurMax;;

(* Générer un but aléatoire *)
let add_rand_goal = fun depth proof ->
  (true, add_remainder (propAleatoire depth) proof);;

(* Générer un contexte aléatoire *)
let add_rand_cont = fun max_prop_depth hyp_quantity proof ->
  let rec it = fun cont acc->
    if cont > 0 then
      it (cont-1) (add_hyp (propAleatoire max_prop_depth) acc)
    else
      acc in
  let new_proof = it hyp_quantity proof in
  (true, new_proof);;

let get_rand_cont = fun max_prp_dp hp->
  add_rand_cont max_prp_dp hp empty_proof;;

(* Générer un but garanti prouvable à partir d'un contexte *)


(* Tester les performances du backtrack *)
let test_backtrack_perfs = fun max_test_depth ->
  ();;
