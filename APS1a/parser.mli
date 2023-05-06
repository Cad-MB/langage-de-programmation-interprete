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
  | VAR
  | PROC
  | SET
  | IFBIG
  | WHILE
  | CALL
  | IF
  | AND
  | OR
  | BOOL
  | INT
  | VARSMALL
  | ADR

val prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.prog
