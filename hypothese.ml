(* Hypothese.ml *)

type proposition = Name of string
  | Implies of proposition * proposition
  | True
  | False
  | Negation of proposition;;

type hypothesis = {
    id: int;
    prop: proposition;
  };;

type proof = {
    hypos: hypothesis list;
    remainder: proposition list;
  };;
