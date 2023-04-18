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
  | ASTBool -> Printf.printf"bool"
  | ASTInt -> Printf.printf"int"
  | ASTTypeFunc(tl, tr) -> (
    Printf.printf"typeFunc([";
    print_typlist tl;
    Printf.printf"], ";
    print_typ tr;
    Printf.printf")";
  )
and print_typlist tl =
  match tl with
  | ASTType t -> print_typ t;
  | ASTTypes(t, ts) -> (
    print_typ t;
    Printf.printf ", ";
    print_typlist ts;
  )

  let rec print_arg ar =
    match ar with
    | ASTArg(x, t) -> (
      Printf.printf"(%s, " x;
      print_typ t;
      Printf.printf")";
    )
  
  and print_arglist al =
    match al with
    | [] -> ()
    | [ar] -> print_arg ar
    | ar::al -> (
      print_arg ar;
      Printf.printf", ";
      print_arglist al
    )
  
let rec print_expr e =
  match e with
  | ASTNum n -> Printf.printf"num(%d)" n
  | ASTId x -> Printf.printf"id(%s)" x
  | ASTIf(cond, ver, alt) -> (
    Printf.printf"if(";
    print_expr cond;
    Printf.printf", ";
    print_expr ver;
    Printf.printf", ";
    print_expr alt;
    Printf.printf")";
  )
  | ASTAnd(a, b) -> (
    Printf.printf"and(";
    print_expr a;
    Printf.printf", ";
    print_expr b;
    Printf.printf")";
  )
  | ASTOr(a, b) -> (
    Printf.printf"or(";
    print_expr a;
    Printf.printf", ";
    print_expr b;
    Printf.printf")";
  )
  | ASTApp(ex, exl) -> (
    Printf.printf"app(";
	  print_expr ex;
	  Printf.printf", [";
	  print_exprlist exl;
	  Printf.printf"])"
  )
  | ASTLambda(arl, ex) -> (
    Printf.printf"abs([";
	  print_arglist arl;
	  Printf.printf"], ";
	  print_expr ex;
	  Printf.printf")"
  )

and print_exprlist el =
  match el with
  | [] -> ()
  | [e] -> print_expr e
  | e::el -> (
    print_expr e;
    Printf.printf", ";
    print_exprlist el
  )

  let print_def d =
    match d with
    | ASTConst(x, t, e) -> (
      Printf.printf"const(%s, " x;
      print_typ t;
      Printf.printf", ";
      print_expr e;
      Printf.printf")";
    )
    | ASTFun(f, t, al, e) -> (
      Printf.printf"fun(%s, " f;
      print_typ t;
      Printf.printf", [";
      print_arglist al;
      Printf.printf"], ";
      print_expr e;
      Printf.printf")";
    )
    | ASTFunRec(fr, t, al, e) -> (
      Printf.printf"funRec(%s, " fr;
      print_typ t;
      Printf.printf", ";
      print_arglist al;
      Printf.printf", ";
      print_expr e;
      Printf.printf")";
    )

let print_stat s =
  match s with
      ASTEcho e -> (
	Printf.printf("echo(");
	print_expr(e);
	Printf.printf(")")
      )

	
let rec print_cmds cs =
  match cs with
  | ASTStat s -> (
    Printf.printf"stat(";
    print_stat s;
    Printf.printf")";
  )
  | ASTDef(d, dl) -> (
    Printf.printf"def(";
    print_def d;
    Printf.printf"), ";
    print_cmds dl;
  )
	
let print_prog p =
  match p with
  | ASTProg prg -> (
  Printf.printf("prog([");
  print_cmds prg;
  Printf.printf("])")
  )
;;
	
let fname = Sys.argv.(1) in
let ic = open_in fname in
  try
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in
      print_prog p;
      print_string ".\n"
  with Lexer.Eof ->
    exit 0