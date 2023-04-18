open Ast


let rec print_typ t =
  match t with
  | ASTBool -> Printf.printf"ASTBool"
  | ASTInt -> Printf.printf"ASTInt"
  | ASTTypeFunc(tl, tr) -> (
    Printf.printf"ASTTypeFunc([";
    print_typlist tl;
    Printf.printf"], ";
    print_typ tr;
    Printf.printf")";
  )
and print_typlist tl =
  match tl with
  | ASTType t -> (
    Printf.printf"ASTType(";
    print_typ t;
    Printf.printf")";
  )
  | ASTTypes(t, ts) -> (
    print_typ t;
    Printf.printf ", ";
    print_typlist ts;
  )


let rec print_arg ar =
  match ar with
  | ASTArg(x, t) -> (
    Printf.printf"ASTArg(%s, " x;
    print_typ t;
    Printf.printf")";
  )

and print_arglist al =
  match al with
  | [] -> ()
  | [ar] -> print_arg ar
  | ar::al -> (
    print_arg ar;
    print_char ',';
    print_arglist al
  )

let rec print_expr e =
  match e with
  | ASTNum n -> Printf.printf"ASTNum(%d)" n
  | ASTId x -> Printf.printf"ASTId(%s)" x
  | ASTIf(cond, ver, alt) -> (
    Printf.printf"ASTIf(";
    print_expr cond;
    Printf.printf", ";
    print_expr ver;
    Printf.printf", ";
    print_expr alt;
    Printf.printf")";
  )
  | ASTAnd(a, b) -> (
    Printf.printf"ASTAnd(";
    print_expr a;
    Printf.printf", ";
    print_expr b;
    Printf.printf")";
  )
  | ASTOr(a, b) -> (
    Printf.printf"ASTOr(";
    print_expr a;
    Printf.printf", ";
    print_expr b;
    Printf.printf")";
  )
  | ASTApp(ex, exl) -> (
    Printf.printf"ASTApp(";
	  print_expr ex;
	  Printf.printf",[";
	  print_exprlist exl;
	  Printf.printf"])"
  )
  | ASTLambda(arl, ex) -> (
    Printf.printf"ASTLambda([";
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
    print_char ',';
    print_exprlist el
  )



let print_def d =
  match d with
  | ASTConst(x, t, e) -> (
    Printf.printf"ASTConst(%s, " x;
    print_typ t;
    Printf.printf", ";
    print_expr e;
    Printf.printf")";
  )
  | ASTFun(f, t, al, e) -> (
    Printf.printf"ASTFun(%s, " f;
    print_typ t;
    Printf.printf", ";
    print_arglist al;
    Printf.printf", ";
    print_expr e;
    Printf.printf")";
  )
  | ASTFunRec(fr, t, al, e) -> (
    Printf.printf"ASTFunRec(%s, " fr;
    print_typ t;
    Printf.printf", ";
    print_arglist al;
    Printf.printf", ";
    print_expr e;
    Printf.printf")";
  )
  
let print_stat s =
  match s with
  | ASTEcho e -> (
    Printf.printf"ASTEcho(";
    print_expr e;
    Printf.printf")";
  )

let rec print_cmds cs =
  match cs with
  | ASTStat s -> (
    Printf.printf"ASTStat(";
    print_stat s;
    Printf.printf")";
  )
  | ASTDef(d, dl) -> (
    Printf.printf"ASTDef(";
    print_def d;
    Printf.printf"), ";
    print_cmds dl;
  )

let print_prog p =
  match p with
  | ASTProg prg -> (
    Printf.printf("ASTProg([");
    print_cmds prg;
    Printf.printf("])");
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