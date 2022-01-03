(* File lexer.mll *)

{
  open Parser

  exception SyntaxError of string

}

rule token = parse
  | [' ' '\t'] {token lexbuf}
  | '\n' { EOL }
  | ['0'-'9']+ as i { INT (int_of_string i) }
  | [ :alnum: ]+ as s { NAME s }
  | '=>' { IMPLIES }
  | '^' { AND }
  | 'v' { OR }
  | '(' { LPAREN }
  | ')' { RPAREN } 
  | _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }
  

