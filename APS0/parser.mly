%{
(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017                          == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: parser.mly                                                  == *)
(* == Analyse syntaxique                                                   == *)
(* ========================================================================== *)

open Ast

%}
  
%token <int> NUM
%token <string> IDENT
%token LPAR RPAR 
%token LBRA RBRA
%token ECHO
%token SEMCOL COMMA PERD STAR ARROW

%token CONST FUN REC ECHO IF

%token AND OR 

%token BOOL INT 

%type <Ast.prog> prog
%type <Ast.cmds> cmds
%type <Ast.def> def
%type <Ast.typ> typ
%type <Ast.typs> typs
%type <Ast.args> args
%type <Ast.arg> arg
%type <Ast.stat> stat
%type <Ast.expr> expr
%type <Ast.exprs> exprs



%start prog

%%
prog:
LBRA cmds RBRA          { ASTProg($2) }
;

cmds:
  stat                  { ASTStat $1 }
| def SEMCOL cmds       { ASTDef($1, $3) }
;

def:
  CONST IDENT typ expr  { ASTConst($2, $3, $4) }
| FUN IDENT typ LBRA args RBRA expr  { ASTFun($2, $3, $5, $7) }
| FUN REC IDENT typ LBRA args RBRA expr  { ASTFunRec($3, $4, $6, $8) }
;

typ:
  BOOL                  { ASTBool }
| INT                   { ASTInt }
| LPAR typs ARROW typ RPAR  { ASTTypeFunc($2, $4) }
;

typs:
  typ                   { ASTType($1) }
| typ STAR typs         { ASTTypes($1, $3) }
;

args:
  arg                   { [$1] }
| arg COMMA args        { $1::$3 }
;

arg:
  IDENT PERD typ      { ASTArg($1, $3) }
;

stat:
  ECHO expr             { ASTEcho($2) }
;

expr:
  NUM                   { ASTNum($1) }
| IDENT                 { ASTId($1) }
| LPAR IF expr expr expr RPAR   { ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR       { ASTAnd($3, $4) }
| LPAR OR expr expr RPAR        { ASTOr($3, $4) }
| LPAR expr exprs RPAR  { ASTApp($2, $3) }
| LBRA args RBRA expr  { ASTLambda($2, $4) }
;

exprs :
  expr       { [$1] }
| expr exprs { $1::$2 }
;

