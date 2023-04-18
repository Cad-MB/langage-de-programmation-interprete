(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: ast.ml                                                      == *)
(* ==  Arbre de syntaxe abstraite                                          == *)
(* ========================================================================== *)

type typ = ASTBool | ASTInt | ASTTypeFunc of typs * typ (*->*)
and typs = ASTType of typ | ASTTypes of typ * typs

type arg = ASTArg of string * typ
and args = arg list

type expr =
  | ASTNum of int
  | ASTId of string
  | ASTIf of expr * expr * expr
  | ASTAnd of expr * expr
  | ASTOr of expr * expr
  | ASTApp of expr * exprs
  | ASTLambda of args * expr

and exprs = expr list

type cmds =
  | ASTStat of stat
  | ASTDefEtc of def * cmds
  | ASTStatEtc of stat * cmds

and block = ASTBlock of cmds

and stat =
  | ASTEcho of expr
  | ASTSet of string * expr
  | ASTIfBig of expr * block * block
  | ASTWhile of expr * block
  | ASTCall of string * exprs

and def =
  | ASTConst of string * typ * expr
  | ASTFun of string * typ * args * expr
  | ASTFunRec of string * typ * args * expr
  | ASTVar of string * typ
  | ASTProc of string * args * block
  | ASTProcRec of string * args * block

type prog = ASTProg of block
