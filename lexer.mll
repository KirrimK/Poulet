{
  open Parser

  exception Error of string

}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let other = ['_' '-' '@' '&' '~' '#' '$' '%'] 
let letter = lower | upper

let number = digit+
let name = letter (letter | digit | other)* (* on fait �a pour avoir l'�quivalent du [::alnum::] des regexp *)

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
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
