type token =
  | NUM of (int)
  | IDENT of (string)
  | LPAR
  | RPAR
  | LBRA
  | RBRA
  | ECHO
  | SEMCOL
  | COMMA
  | PERD
  | STAR
  | ARROW
  | CONST
  | FUN
  | REC
  | IF
  | AND
  | OR
  | BOOL
  | INT

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
