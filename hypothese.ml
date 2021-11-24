(* Hypothese.ml *)

type proposition = Name of string
  | Implies of proposition * proposition
  | True
  | False;;

type hypothesis = {
    name: string;
    prop: proposition;
  };;
