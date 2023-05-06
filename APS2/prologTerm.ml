(* ========================================================================== *)
(* == UPMC/master/info/4I506 -- Janvier 2016/2017/2018                     == *)
(* == SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021/2022                == *)
(* == Analyse des programmes et sémantiques                                == *)
(* ========================================================================== *)
(* == hello-APS Syntaxe ML                                                 == *)
(* == Fichier: prologTerm.ml                                               == *)
(* ==  Génération de termes Prolog                                         == *)
(* ========================================================================== *)
open Ast

let rec print_typ t =
  match t with
  | ASTBool -> Printf.printf "bool"
  | ASTInt -> Printf.printf "int"
  | ASTTypeFunc (tl, tr) ->
      Printf.printf "typeFunc([";
      print_typlist tl;
      Printf.printf "], ";
      print_typ typ ;  
      Printf.printf(")")
        )
        | ASTTypeVec(typ) -> (
      Printf.printf("typeVec");
      Printf.printf("(");
      print_typ tr;
      Printf.printf ")"

and print_typlist tl =
  match tl with
  | ASTType t -> print_typ t
  | ASTTypes (t, ts) ->
      print_typ t;
      Printf.printf ", ";
      print_typlist ts

let rec print_arg ar =
  match ar with
  | ASTArg (x, t) ->
      Printf.printf "(%s, " x;
      print_typ t;
      Printf.printf ")"

and print_arglist al =
  match al with
  | [] -> ()
  | [ ar ] -> print_arg ar
  | ar :: al ->
      print_arg ar;
      Printf.printf ", ";
      print_arglist al

let rec print_argp ap =
  match ap with
  | ASTArgp (x, t) ->
      Printf.printf "(%s, " x;
      print_typ t;
      Printf.printf ")"
  | ASTArgpVar (x, t) ->
      Printf.printf "(varSmall(%s), " x;
      print_typ t;
      Printf.printf ")"

and print_argplist apl =
  match apl with
  | [] -> ()
  | [ ap ] -> print_argp ap
  | ap :: apl ->
      print_argp ap;
      print_char ',';
      print_argplist apl

let rec print_expr e =
  match e with
  | ASTNum n -> Printf.printf "num(%d)" n
  | ASTId x -> Printf.printf "id(%s)" x
  | ASTIf (cond, ver, alt) ->
      Printf.printf "if(";
      print_expr cond;
      Printf.printf ", ";
      print_expr ver;
      Printf.printf ", ";
      print_expr alt;
      Printf.printf ")"
  | ASTAnd (a, b) ->
      Printf.printf "and(";
      print_expr a;
      Printf.printf ", ";
      print_expr b;
      Printf.printf ")"
  | ASTOr (a, b) ->
      Printf.printf "or(";
      print_expr a;
      Printf.printf ", ";
      print_expr b;
      Printf.printf ")"
  | ASTApp (ex, exl) ->
      Printf.printf "app(";
      print_expr ex;
      Printf.printf ", [";
      print_exprlist exl;
      Printf.printf "])"
  | ASTLambda (arl, ex) ->
      Printf.printf "abs([";
      print_arglist arl;
      Printf.printf "], ";
      print_expr ex;
      Printf.printf ")"
      | ASTLen(e) -> (
        Printf.printf"len(";
        print_expr e;
        Printf.printf")"
    )
    | ASTNth(e1, e2) -> (
  Printf.printf"nth(";
  print_expr e1;
  Printf.printf",";
  print_expr e2;
  Printf.printf")"  
      )
    | ASTAlloc(e) -> (
        Printf.printf"alloc(";
        print_expr e;
        Printf.printf")"
    )
    | ASTVset(e1, e2, e3) -> (
  Printf.printf"vset(";
  print_expr e1;
  Printf.printf",";
  print_expr e2;
  Printf.printf",";
  print_expr e3;
  Printf.printf")"  
      )

and print_exprlist el =
  match el with
  | [] -> ()
  | [ e ] -> print_expr e
  | e :: el ->
      print_expr e;
      Printf.printf ", ";
      print_exprlist el
      )
      let rec print_lval l =
        match l with
          ASTLvId (x) -> Printf.printf"id(%s)" x
        | ASTLvVar(lv, e) -> (
        Printf.printf"nth(";
        print_lval lv;
        Printf.printf",";
        print_expr e;
        Printf.printf")"
          )
          

let rec print_exprp ep =
  match ep with
  | ASTExpr e -> print_expr e
  | ASTAdr x -> Printf.printf "adr(%s)" x

and print_exprplist epl =
  match epl with
  | [] -> ()
  | [ ep ] -> print_exprp ep
  | ep :: epl ->
      print_exprp ep;
      print_char ',';
      print_exprplist epl

let rec print_cmds cs =
  match cs with
  | ASTStat s ->
      Printf.printf "stat(";
      print_stat s;
      Printf.printf ")"
  | ASTDefEtc (d, cl) ->
      Printf.printf "def(";
      print_def d;
      Printf.printf "), ";
      print_cmds cl
  | ASTStatEtc (s, cl) ->
      Printf.printf "stat(";
      print_stat s;
      Printf.printf "), ";
      print_cmds cl

and print_block bk =
  match bk with
  | ASTBlock cs ->
      Printf.printf "block([";
      print_cmds cs;
      Printf.printf "])"

and print_def d =
  match d with
  | ASTConst (x, t, e) ->
      Printf.printf "const(%s, " x;
      print_typ t;
      Printf.printf ", ";
      print_expr e;
      Printf.printf ")"
  | ASTFun (f, t, al, e) ->
      Printf.printf "fun(%s, " f;
      print_typ t;
      Printf.printf ", [";
      print_arglist al;
      Printf.printf "], ";
      print_expr e;
      Printf.printf ")"
  | ASTFunRec (fr, t, al, e) ->
      Printf.printf "funRec(%s, " fr;
      print_typ t;
      Printf.printf ", [";
      print_arglist al;
      Printf.printf "], ";
      print_expr e;
      Printf.printf ")"
  | ASTVar (x, t) ->
      Printf.printf "var(%s, " x;
      print_typ t;
      Printf.printf ")"
  | ASTProc (p, apl, bk) ->
      Printf.printf "proc(%s, [" p;
      print_argplist apl;
      Printf.printf "], ";
      print_block bk;
      Printf.printf ")"
  | ASTProcRec (pr, apl, bk) ->
      Printf.printf "procRec(%s, [" pr;
      print_argplist apl;
      Printf.printf "], ";
      print_block bk;
      Printf.printf ")"

and print_stat s =
  match s with
  | ASTEcho e ->
      Printf.printf "echo(";
      print_expr e;
      Printf.printf ")"
  | ASTSet (x, e) ->
      Printf.printf "set(%s, " x;
      print_lval e;
      Printf.printf ")"
  | ASTIfBig (cond, ver, alt) ->
      Printf.printf "ifBig(";
      print_expr cond;
      Printf.printf ", ";
      print_block ver;
      Printf.printf ", ";
      print_block alt;
      Printf.printf ")"
  | ASTWhile (cond, loop) ->
      Printf.printf "while(";
      print_expr cond;
      Printf.printf ", ";
      print_block loop;
      Printf.printf ")"
  | ASTCall (p, epl) ->
      Printf.printf "call(%s, [" p;
      print_exprplist epl;
      Printf.printf "])"

let print_prog p =
  match p with
  | ASTProg bk ->
      Printf.printf "prog(";
      print_block bk;
      Printf.printf ")"
;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
try
  let lexbuf = Lexing.from_channel ic in
  let p = Parser.prog Lexer.token lexbuf in
  print_prog p;
  print_string ".\n"
with Lexer.Eof -> exit 0
