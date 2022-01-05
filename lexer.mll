{
  open Parser

  exception Error of string

}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper

let number = digit+
let name = letter (letter | digit)* (* on fait ça pour avoir l'équivalent du [::alnum::] dans les regexp *)

rule token = parse
  | [' ' '\t'] {token lexbuf}
  | '\n' { EOL }
  | name as s { NAME s }
  | "=>" { IMPLIES }
  | '^' { AND }
  | '|' { OR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
