(* cli.ml interface*)

open Hypothese;;


let show_hypo h = Printf.printf "%s :  %d" h.name h.id

let rec show_prop = fun p ->
match p with
  |Name n -> Printf.printf "%s" n (* to string *) 
  |True t-> Printf.printf "True" (* symbole mathématique*)
  |False f->Printf.printf "False"
  |Implies (p1,p2) -> Printf.printf " %s => %s " p1 p2


