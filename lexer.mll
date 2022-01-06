{
  open Parser

  exception Error of string

}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper

let number = digit+
let name = letter (letter | digit)* (* on fait ça pour avoir l'équivalent du [::alnum::] des regexp *)

rule token = parse
  | [' ' '\t'] {token lexbuf}
  | '\n' { EOL }
  | "NOT" { NOT }
  | "TRUE" { TRUE }
  | "FALSE" { FALSE }
  | name as s { NAME s }
  | "=>" { IMPLIES }
  | "<=>" {EQUI}
  | '^' { AND }
  | '|' { OR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
