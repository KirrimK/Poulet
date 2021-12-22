(* Tests.ml *)

open Strategies;;

let test1 = add_remainder empty_proof (make_prop ["A"; "B"; "=>"]);;

let test1_res = add_hyp (add_remainder empty_proof (make_prop ["B"])) (make_prop ["A"]);;

let test = fun name funct dep arr ->
  Printf.printf "%s test %s\n" name (if funct dep = (true, arr) then "success" else "fail");;

test "intro 1" intro test1 test1_res;;

let test2 = add_hyp (add_remainder empty_proof (make_prop ["A"])) (make_prop ["A"]);;

let test2_res = add_hyp (add_remainder empty_proof (make_prop ["True"])) (make_prop ["A"]);;

test "assumption 1" assumption test2 test2_res;;

let test3 = add_hyp (add_remainder empty_proof (make_prop ["B"; "C"; "=>"])) (make_prop ["A"]);;

let test3_res = add_hyp (add_hyp (add_remainder empty_proof (make_prop ["C"])) (make_prop ["A"])) (make_prop ["B"]);;

test "intro 2" intro test3 test3_res;;

let test4 = add_hyp (add_remainder empty_proof (make_prop ["B"])) (make_prop ["A"; "B"; "=>"]);;

let test4_res = add_hyp (add_remainder empty_proof (make_prop ["A"])) (make_prop ["A"; "B"; "=>"]);;

test "apply 1" (fun dep -> apply 0 dep) test4 test4_res;;

let test5 = add_remainder empty_proof (make_prop ["A"; "B"; "^"]);;

let test5_res = add_remainder (add_remainder empty_proof (make_prop ["B"])) (make_prop ["A"]);;

test "andsplit" andsplit test5 test5_res;;
