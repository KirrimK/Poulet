(* Tests.ml *)

open Strategies;;

let h1 = {hypos=[];remainder=[Implies(Name("A"),Name("B"))]} ;;
#use "strategies.ml";;
if (intro h1) = (true,{hypos=[{id=0;prop=Name("A")}];remainder=[Name("B")]}) then Printf.printf "\nintro test 1 success" else Printf.printf "\nintro test 1 fail";;

if (assumption {hypos=[{id=0;prop=Name("A")}];remainder=[Name("B")]}) = (false,{hypos=[{id=0;prop=Name("A")}];remainder=[Name("B")]}) then Printf.printf "assumption test 1 success" else Printf.printf "assumption test 1 fail";;
