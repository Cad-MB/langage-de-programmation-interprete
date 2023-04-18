(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type typ = 
    ASTBool
  | ASTInt
  | ASTTypeFunc of typs * typ (*->*)
and typs = 
    ASTType of typ 
  | ASTTypes of typ * typs

type arg = 
    ASTArg of string * typ
and args = arg list

type expr =
    ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTApp of expr * exprs
  | ASTLambda of args * expr
and exprs = expr list 

type stat =
  ASTEcho of expr

type def = 
    ASTConst of string * typ * expr
  | ASTFun of string * typ * args * expr
  | ASTFunRec of string * typ * args * expr

type cmds = 
    ASTStat of stat
  | ASTDef of def * cmds

type prog =
    ASTProg of cmds