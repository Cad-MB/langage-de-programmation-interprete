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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
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

# 45 "parser.ml"
let yytransl_const = [|
  259 (* LPAR *);
  260 (* RPAR *);
  261 (* LBRA *);
  262 (* RBRA *);
  263 (* ECHO *);
  264 (* SEMCOL *);
  265 (* COMMA *);
  266 (* PERD *);
  267 (* STAR *);
  268 (* ARROW *);
  269 (* CONST *);
  270 (* FUN *);
  271 (* REC *);
  272 (* VAR *);
  273 (* PROC *);
  274 (* SET *);
  275 (* IFBIG *);
  276 (* WHILE *);
  277 (* CALL *);
  278 (* IF *);
  279 (* AND *);
  280 (* OR *);
  281 (* BOOL *);
  282 (* INT *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\011\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\004\000\005\000\005\000\
\006\000\006\000\007\000\008\000\008\000\008\000\008\000\008\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\010\000\
\010\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\005\000\001\000\003\000\
\001\000\003\000\003\000\002\000\003\000\004\000\003\000\003\000\
\001\000\001\000\006\000\005\000\005\000\004\000\004\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\034\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\025\000\026\000\000\000\000\000\020\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\013\000\000\000\000\000\000\000\
\009\000\000\000\000\000\021\000\000\000\023\000\000\000\024\000\
\004\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\006\000\000\000\000\000\000\000\000\000\
\022\000\033\000\000\000\000\000\000\000\030\000\019\000\031\000\
\018\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\028\000\029\000\016\000\000\000\000\000\000\000\010\000\000\000\
\027\000\014\000\007\000\000\000\011\000\008\000"

let yydgoto = "\002\000\
\004\000\015\000\016\000\066\000\067\000\041\000\042\000\017\000\
\055\000\056\000\005\000"

let yysindex = "\009\000\
\012\255\000\000\058\255\000\000\000\000\034\255\017\255\013\255\
\018\255\014\255\032\255\034\255\034\255\042\255\044\255\038\255\
\048\255\000\000\000\000\040\255\050\255\000\000\001\255\001\255\
\057\255\001\255\055\255\059\255\034\255\012\255\012\255\034\255\
\000\000\058\255\058\255\034\255\034\255\034\255\034\255\056\255\
\064\255\071\255\001\255\000\000\000\000\034\255\068\255\001\255\
\000\000\050\255\076\255\000\000\012\255\000\000\034\255\000\000\
\000\000\000\000\034\255\034\255\034\255\078\255\001\255\034\255\
\050\255\072\255\073\255\000\000\050\255\079\255\080\255\050\255\
\000\000\000\000\034\255\084\255\085\255\000\000\000\000\000\000\
\000\000\001\255\001\255\086\255\050\255\012\255\087\255\091\255\
\000\000\000\000\000\000\092\255\034\255\093\255\000\000\012\255\
\000\000\000\000\000\000\034\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\094\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\095\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\043\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\090\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\234\255\000\000\241\255\008\000\209\255\000\000\000\000\
\250\255\222\255\227\255"

let yytablesize = 102
let yytable = "\022\000\
\053\000\054\000\071\000\043\000\062\000\030\000\031\000\046\000\
\047\000\001\000\049\000\057\000\058\000\039\000\024\000\027\000\
\003\000\081\000\023\000\026\000\074\000\084\000\052\000\073\000\
\087\000\044\000\045\000\025\000\028\000\059\000\060\000\061\000\
\070\000\029\000\018\000\019\000\020\000\094\000\021\000\068\000\
\018\000\019\000\020\000\032\000\021\000\034\000\032\000\079\000\
\032\000\033\000\032\000\040\000\075\000\076\000\077\000\035\000\
\095\000\080\000\048\000\050\000\051\000\036\000\037\000\038\000\
\006\000\063\000\101\000\092\000\088\000\064\000\007\000\008\000\
\069\000\009\000\010\000\011\000\012\000\013\000\014\000\065\000\
\072\000\078\000\082\000\085\000\083\000\086\000\099\000\089\000\
\090\000\091\000\000\000\093\000\096\000\102\000\097\000\098\000\
\000\000\000\000\100\000\003\000\017\000\015\000"

let yycheck = "\006\000\
\030\000\031\000\050\000\003\001\039\000\012\000\013\000\023\000\
\024\000\001\000\026\000\034\000\035\000\020\000\002\001\002\001\
\005\001\065\000\002\001\002\001\055\000\069\000\029\000\053\000\
\072\000\025\001\026\001\015\001\015\001\036\000\037\000\038\000\
\048\000\002\001\001\001\002\001\003\001\085\000\005\001\046\000\
\001\001\002\001\003\001\002\001\005\001\008\001\004\001\063\000\
\006\001\006\001\008\001\002\001\059\000\060\000\061\000\008\001\
\086\000\064\000\002\001\005\001\002\001\022\001\023\001\024\001\
\007\001\010\001\096\000\083\000\075\000\006\001\013\001\014\001\
\005\001\016\001\017\001\018\001\019\001\020\001\021\001\009\001\
\005\001\004\001\011\001\005\001\012\001\006\001\093\000\004\001\
\004\001\082\000\255\255\006\001\006\001\100\000\004\001\004\001\
\255\255\255\255\006\001\006\001\006\001\012\001"

let yynames_const = "\
  LPAR\000\
  RPAR\000\
  LBRA\000\
  RBRA\000\
  ECHO\000\
  SEMCOL\000\
  COMMA\000\
  PERD\000\
  STAR\000\
  ARROW\000\
  CONST\000\
  FUN\000\
  REC\000\
  VAR\000\
  PROC\000\
  SET\000\
  IFBIG\000\
  WHILE\000\
  CALL\000\
  IF\000\
  AND\000\
  OR\000\
  BOOL\000\
  INT\000\
  "

let yynames_block = "\
  NUM\000\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 47 "parser.mly"
               ( ASTProg(_1) )
# 215 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 51 "parser.mly"
                   ( ASTBlock(_2) )
# 222 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 55 "parser.mly"
                       ( ASTStat(_1) )
# 229 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 56 "parser.mly"
                        ( ASTDefEtc(_1, _3) )
# 237 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 57 "parser.mly"
                     ( ASTStatEtc(_1, _3) )
# 245 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 61 "parser.mly"
                               ( ASTConst(_2, _3, _4) )
# 254 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 62 "parser.mly"
                                       ( ASTFun(_2, _3, _5, _7) )
# 264 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                                         ( ASTFunRec(_3, _4, _6, _8) )
# 274 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 64 "parser.mly"
                             ( ASTVar(_2, _3) )
# 282 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 65 "parser.mly"
                                      ( ASTProc(_2, _4, _6) )
# 291 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 66 "parser.mly"
                                        ( ASTProcRec(_3, _5, _7) )
# 300 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    Obj.repr(
# 70 "parser.mly"
                       ( ASTBool )
# 306 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                        ( ASTInt )
# 312 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    Obj.repr(
# 72 "parser.mly"
                            ( ASTTypeFunc(_2, _4) )
# 320 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 76 "parser.mly"
                       ( ASTType(_1) )
# 327 "parser.ml"
               : Ast.typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typs) in
    Obj.repr(
# 77 "parser.mly"
                        ( ASTTypes(_1, _3) )
# 335 "parser.ml"
               : Ast.typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 81 "parser.mly"
                       ( [_1] )
# 342 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 82 "parser.mly"
                        ( _1::_3 )
# 350 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 86 "parser.mly"
                     ( ASTArg(_1, _3) )
# 358 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 90 "parser.mly"
                   ( ASTEcho(_2) )
# 365 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 91 "parser.mly"
                      ( ASTSet(_2, _3) )
# 373 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 92 "parser.mly"
                          ( ASTIfBig(_2, _3, _4) )
# 382 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 93 "parser.mly"
                       ( ASTWhile(_2, _3) )
# 390 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprs) in
    Obj.repr(
# 94 "parser.mly"
                       ( ASTCall(_2, _3) )
# 398 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 98 "parser.mly"
                  ( ASTNum(_1) )
# 405 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 99 "parser.mly"
                    ( ASTId(_1) )
# 412 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 100 "parser.mly"
                               ( ASTIf(_3, _4, _5) )
# 421 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 101 "parser.mly"
                             ( ASTAnd(_3, _4) )
# 429 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 102 "parser.mly"
                            ( ASTOr(_3, _4) )
# 437 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprs) in
    Obj.repr(
# 103 "parser.mly"
                           ( ASTApp(_2, _3) )
# 445 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 104 "parser.mly"
                           ( ASTLambda(_2, _4) )
# 453 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 108 "parser.mly"
              ( [_1] )
# 460 "parser.ml"
               : Ast.exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprs) in
    Obj.repr(
# 109 "parser.mly"
                  ( _1::_2 )
# 468 "parser.ml"
               : Ast.exprs))
(* Entry prog *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let prog (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.prog)
