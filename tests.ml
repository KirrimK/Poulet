(* Tests.ml *)

open Strategies;;

let h1 = {hypos=[];remainder=[Implies(Name("A"),Name("B"))]} ;;

if (intro h1) = (true,{hypos=[{id=0;prop=Name("A")}];remainder=[Name("B")]}) then Printf.printf "intro test 1 success\n" else Printf.printf "intro test 1 fail\n";;

if (assumption {hypos=[{id=0;prop=Name("A")}];remainder=[Name("B")]}) = (false,{hypos=[{id=0;prop=Name("A")}];remainder=[Name("B")]}) then Printf.printf "assumption test 1 success\n" else Printf.printf "assumption test 1 fail\n";;

let h2 = {hypos=[{id=0;prop=Name("A")}];remainder=[Name("A")]};;

if (intro h2) = (false,h2) then Printf.printf "intro test 2 success\n" else Printf.printf "intro test 2 fail\n";;

if (assumption h2) = (true,{hypos=[{id=0;prop=Name("A")}];remainder=[True]}) then Printf.printf "assumption test 2 success\n" else Printf.printf "assumption test 2 fail\n";;

let h3 = {hypos=[{id=0;prop=Name("A")}];remainder=[Implies(Name("B"),Name("C"))]};;

if (intro h3) = (true,{hypos=[{id=1;prop=Name("B")};{id=0;prop=Name("A")}];remainder=[Name("C")]}) then Printf.printf "intro test 3 success\n" else Printf.printf "intro test 3 fail\n";;

if (assumption h3) = (false,h3) then Printf.printf "assumption test 3 success\n" else Printf.printf "assumption test 3 fail\n";;

let h4 = {hypos=[{id=0;prop=Implies(Name("A"),Name("B"))}];remainder=[Name("B")]};;

if (apply 0 h4) = (true,{hypos=[{id=0;prop=Implies(Name("A"),Name("B"))}];remainder=[Name("A")]}) then Printf.printf "apply test success\n" else Printf.printf "apply test fail\n";;
