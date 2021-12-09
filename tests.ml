(* Tests.ml *)

open Strategies;;
#use "strategies.ml";;

let h1 = {hypos=[];remainder=[Implies(Name("A"),Name("B"))]} ;;

if (intro h1) = (true,{hypos=[{id=0;prop=Name("A")}];remainder=[Name("B")]}) then Printf.printf "\nintro test 1 success" else Printf.printf "\nintro test 1 fail";;

if (assumption {hypos=[{id=0;prop=Name("A")}];remainder=[Name("B")]}) = (false,{hypos=[{id=0;prop=Name("A")}];remainder=[Name("B")]}) then Printf.printf "assumption test 1 success" else Printf.printf "assumption test 1 fail";;

let h2 = {hypos=[{id=0;prop=Name("A")}];remainder=[Name("A")]};;

if (intro h2) = (false,h2) then Printf.printf "intro test 2 success" else Printf.printf "intro test 2 fail";;

if (assumption h2) = (true,{hypos=[{id=0;prop=Name("A")}];remainder=[True]}) then Printf.printf "assumption test 2 success" else Printf.printf "assumption test 2 fail";;

let h3 = {hypos=[{id=0;prop=Name("A")}];remainder=[Implies(Name("B"),Name("C"))]};;

if (intro h3) = (true,{hypos=[{id=1;prop=Name("B")};{id=0;prop=Name("A")}];remainder=[Name("C")]}) then Printf.printf "intro test 3 success" else Printf.printf "intro test 3 fail";;

if (assumption h3) = (false,h3) then Printf.printf "assumption test 3 success" else Printf.printf "assumption test 3 fail";;
