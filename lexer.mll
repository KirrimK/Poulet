{
  open Parser

  exception SyntaxError of string

}

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper

let number = digit+
let name = letter (letter | digit)* (* on fait �a pour avoir l'�quivalent du [::alnum::] dans les regexp *)

rule token = parse
  | [' ' '\t'] {token lexbuf}
  | '\n' { EOL }
  | name as s { NAME s }
  | "=>" { IMPLIES }
  | '^' { AND }
  | 'v' { OR }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | eof { EOF }
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
