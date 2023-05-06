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

%token CONST FUN REC VAR PROC ECHO SET IFBIG WHILE CALL

%token LEN NTH ALLOC VSET VEC

%token IF AND OR 

%token BOOL INT

%token VARSMALL ADR

%token RETURN

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
%type <Ast.block> block



%start prog

%%
prog:
  block									{ ASTProg($1) }
;

ret:
  RETURN expr           { ASTRet($2) }

block:
  LBRA cmds RBRA				{ ASTBlock($2) }
;

cmds:
  stat                  { ASTStat($1) }
| def SEMCOL cmds       { ASTDefEtc($1, $3) }
|	stat SEMCOL cmds			{ ASTStatEtc($1, $3) }
| ret                   { ASTReturn($1) }
;

def:
  CONST IDENT typ expr										{ ASTConst($2, $3, $4) }
| FUN IDENT typ LBRA args RBRA expr				{ ASTFun($2, $3, $5, $7) }
| FUN REC IDENT typ LBRA args RBRA expr		{ ASTFunRec($3, $4, $6, $8) }
|	VAR IDENT typ														{ ASTVar($2, $3) }
|	PROC IDENT LBRA argsp RBRA block				{ ASTProc($2, $4, $6) }
| PROC REC IDENT LBRA argsp RBRA block		{ ASTProcRec($3, $5, $7) }
| FUN IDENT typ LCRO argsp RCRO block     { ASTFunb($2,$3,$5,$7) }
| FUN REC IDENT typ LCRO argsp RCRO block { ASTFunRecb($3,$4,$6,$8) }
;

typ:
  BOOL                  { ASTBool }
| INT                   { ASTInt }
| LPAR typs ARROW typ RPAR  { ASTTypeFunc($2, $4) }
| LPAR VEC type RPAR             { ASTTypeVec($3) }
;

typs:
  typ                   { ASTType($1) }
| typ STAR typs         { ASTTypes($1, $3) }
;

argsp:
  argp                  { [$1] }
| argp COMMA argsp      { $1::$3 }
;

argp:
  IDENT PERD typ            { ASTArgp($1, $3) }
| VARSMALL IDENT PERD typ   { ASTArgpVar($2, $4) }
;

args:
  arg                   { [$1] }
| arg COMMA args        { $1::$3 }
;

arg:
  IDENT PERD typ        { ASTArg($1, $3) }
;

stat:
  ECHO expr									{ ASTEcho($2) }
|	SET lval expr						{ ASTSet($2, $3) }
|	IFBIG expr block block		{ ASTIfBig($2, $3, $4) }
| WHILE expr block					{ ASTWhile($2, $3) }
|	CALL IDENT exprsp					{ ASTCall($2, $3) }
;

exprsp:
  exprp									{ [$1] }
|	exprp exprsp          { $1::$2 }
;

exprp:
  expr                  { ASTExpr($1) }
| LPAR ADR IDENT RPAR   { ASTAdr($3) }
;

expr:
  NUM														{ ASTNum($1) }
| IDENT													{ ASTId($1) }
| LPAR IF expr expr expr RPAR		{ ASTIf($3, $4, $5) }
| LPAR AND expr expr RPAR				{ ASTAnd($3, $4) }
| LPAR OR expr expr RPAR				{ ASTOr($3, $4) }
| LPAR expr exprs RPAR					{ ASTApp($2, $3) }
| LBRA args RBRA expr						{ ASTLambda($2, $4) }
| LPAR LEN expr RPAR            { ASTLen($3) }
| LPAR NTH expr expr RPAR       { ASTNth($3, $4) }
| LPAR ALLOC expr RPAR          { ASTAlloc($3) }
| LPAR VSET expr expr expr RPAR { ASTVset($3, $4, $5) }
;

exprs :
  expr									{ [$1] }
|	expr exprs						{ $1::$2 }
;

lval:
  IDENT                         { ASTLvId($1) }
| LPAR NTH lval expr RPAR       { ASTLvVar($3, $4) }
;