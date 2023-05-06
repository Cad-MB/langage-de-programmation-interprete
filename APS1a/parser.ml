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

# 47 "parser.ml"
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
  283 (* VARSMALL *);
  284 (* ADR *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\011\000\002\000\002\000\002\000\003\000\003\000\003\000\
\003\000\003\000\003\000\004\000\004\000\004\000\005\000\005\000\
\012\000\012\000\013\000\013\000\006\000\006\000\007\000\008\000\
\008\000\008\000\008\000\008\000\014\000\014\000\015\000\015\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\010\000\
\010\000\000\000"

let yylen = "\002\000\
\001\000\003\000\001\000\003\000\003\000\004\000\007\000\008\000\
\003\000\006\000\007\000\001\000\001\000\005\000\001\000\003\000\
\001\000\003\000\003\000\004\000\001\000\003\000\003\000\002\000\
\003\000\004\000\003\000\003\000\001\000\002\000\001\000\004\000\
\001\000\001\000\006\000\005\000\005\000\004\000\004\000\001\000\
\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\042\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\033\000\034\000\000\000\000\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\012\000\013\000\000\000\000\000\000\000\
\009\000\000\000\000\000\025\000\000\000\027\000\000\000\031\000\
\028\000\000\000\004\000\005\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\006\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\026\000\000\000\
\030\000\000\000\000\000\000\000\041\000\038\000\023\000\039\000\
\022\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\036\000\037\000\016\000\000\000\
\000\000\000\000\019\000\000\000\010\000\018\000\000\000\032\000\
\035\000\014\000\007\000\000\000\020\000\011\000\008\000"

let yydgoto = "\002\000\
\004\000\015\000\016\000\069\000\070\000\041\000\042\000\017\000\
\039\000\065\000\005\000\076\000\077\000\057\000\058\000"

let yysindex = "\009\000\
\012\255\000\000\065\255\000\000\000\000\066\255\017\255\007\255\
\019\255\047\255\026\255\066\255\066\255\037\255\018\255\042\255\
\046\255\000\000\000\000\041\255\064\255\000\000\022\255\022\255\
\072\255\022\255\070\255\078\255\066\255\012\255\012\255\088\255\
\000\000\065\255\065\255\066\255\066\255\066\255\066\255\084\255\
\082\255\086\255\022\255\000\000\000\000\066\255\091\255\022\255\
\000\000\011\255\092\255\000\000\012\255\000\000\013\255\000\000\
\000\000\088\255\000\000\000\000\066\255\066\255\066\255\066\255\
\094\255\022\255\066\255\064\255\089\255\090\255\000\000\064\255\
\096\255\093\255\102\255\099\255\097\255\011\255\000\000\105\255\
\000\000\066\255\104\255\107\255\000\000\000\000\000\000\000\000\
\000\000\022\255\022\255\103\255\064\255\022\255\106\255\012\255\
\011\255\108\255\109\255\111\255\000\000\000\000\000\000\113\255\
\066\255\112\255\000\000\022\255\000\000\000\000\012\255\000\000\
\000\000\000\000\000\000\066\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\114\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\115\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\045\255\000\000\000\000\000\000\000\000\000\000\118\255\
\000\000\000\000\000\000\000\000\100\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\117\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\025\000\000\000\235\255\029\000\192\255\000\000\000\000\
\250\255\060\000\237\255\179\255\000\000\067\000\000\000"

let yytablesize = 125
let yytable = "\022\000\
\098\000\046\000\047\000\089\000\049\000\030\000\031\000\092\000\
\024\000\001\000\053\000\054\000\074\000\018\000\019\000\020\000\
\003\000\021\000\023\000\110\000\026\000\025\000\052\000\033\000\
\043\000\056\000\073\000\029\000\106\000\061\000\062\000\063\000\
\064\000\079\000\036\000\037\000\038\000\075\000\032\000\071\000\
\080\000\018\000\019\000\020\000\087\000\021\000\044\000\045\000\
\027\000\034\000\029\000\056\000\029\000\035\000\082\000\083\000\
\084\000\064\000\059\000\060\000\088\000\028\000\036\000\037\000\
\038\000\040\000\018\000\019\000\020\000\104\000\021\000\006\000\
\107\000\048\000\050\000\100\000\109\000\007\000\008\000\051\000\
\009\000\010\000\011\000\012\000\013\000\014\000\117\000\067\000\
\018\000\019\000\055\000\118\000\021\000\066\000\068\000\072\000\
\078\000\086\000\115\000\090\000\093\000\091\000\094\000\095\000\
\096\000\097\000\099\000\101\000\105\000\119\000\102\000\015\000\
\112\000\111\000\113\000\108\000\114\000\116\000\103\000\003\000\
\021\000\040\000\017\000\085\000\081\000"

let yycheck = "\006\000\
\078\000\023\000\024\000\068\000\026\000\012\000\013\000\072\000\
\002\001\001\000\030\000\031\000\002\001\001\001\002\001\003\001\
\005\001\005\001\002\001\097\000\002\001\015\001\029\000\006\001\
\003\001\032\000\048\000\002\001\093\000\036\000\037\000\038\000\
\039\000\053\000\022\001\023\001\024\001\027\001\002\001\046\000\
\028\001\001\001\002\001\003\001\066\000\005\001\025\001\026\001\
\002\001\008\001\006\001\058\000\008\001\008\001\061\000\062\000\
\063\000\064\000\034\000\035\000\067\000\015\001\022\001\023\001\
\024\001\002\001\001\001\002\001\003\001\091\000\005\001\007\001\
\094\000\002\001\005\001\082\000\096\000\013\001\014\001\002\001\
\016\001\017\001\018\001\019\001\020\001\021\001\108\000\006\001\
\001\001\002\001\003\001\111\000\005\001\010\001\009\001\005\001\
\005\001\004\001\105\000\011\001\005\001\012\001\010\001\002\001\
\006\001\009\001\002\001\004\001\006\001\116\000\004\001\012\001\
\004\001\006\001\004\001\010\001\004\001\006\001\090\000\006\001\
\006\001\004\001\006\001\064\000\058\000"

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
  VARSMALL\000\
  ADR\000\
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
# 49 "parser.mly"
                ( ASTProg(_1) )
# 235 "parser.ml"
               : Ast.prog))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.cmds) in
    Obj.repr(
# 53 "parser.mly"
                    ( ASTBlock(_2) )
# 242 "parser.ml"
               : Ast.block))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stat) in
    Obj.repr(
# 57 "parser.mly"
                        ( ASTStat(_1) )
# 249 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.def) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 58 "parser.mly"
                        ( ASTDefEtc(_1, _3) )
# 257 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.stat) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.cmds) in
    Obj.repr(
# 59 "parser.mly"
                     ( ASTStatEtc(_1, _3) )
# 265 "parser.ml"
               : Ast.cmds))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 63 "parser.mly"
                                ( ASTConst(_2, _3, _4) )
# 274 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 64 "parser.mly"
                                       ( ASTFun(_2, _3, _5, _7) )
# 284 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : Ast.typ) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 65 "parser.mly"
                                         ( ASTFunRec(_3, _4, _6, _8) )
# 294 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 66 "parser.mly"
                             ( ASTVar(_2, _3) )
# 302 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'argsp) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 67 "parser.mly"
                                      ( ASTProc(_2, _4, _6) )
# 311 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : string) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'argsp) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 68 "parser.mly"
                                        ( ASTProcRec(_3, _5, _7) )
# 320 "parser.ml"
               : Ast.def))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                        ( ASTBool )
# 326 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    Obj.repr(
# 73 "parser.mly"
                        ( ASTInt )
# 332 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Ast.typs) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.typ) in
    Obj.repr(
# 74 "parser.mly"
                            ( ASTTypeFunc(_2, _4) )
# 340 "parser.ml"
               : Ast.typ))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 78 "parser.mly"
                        ( ASTType(_1) )
# 347 "parser.ml"
               : Ast.typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.typ) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typs) in
    Obj.repr(
# 79 "parser.mly"
                        ( ASTTypes(_1, _3) )
# 355 "parser.ml"
               : Ast.typs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'argp) in
    Obj.repr(
# 83 "parser.mly"
                        ( [_1] )
# 362 "parser.ml"
               : 'argsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'argp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'argsp) in
    Obj.repr(
# 84 "parser.mly"
                        ( _1::_3 )
# 370 "parser.ml"
               : 'argsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 88 "parser.mly"
                            ( ASTArgp(_1, _3) )
# 378 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 89 "parser.mly"
                            ( ASTArgpVar(_2, _4) )
# 386 "parser.ml"
               : 'argp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.arg) in
    Obj.repr(
# 93 "parser.mly"
                        ( [_1] )
# 393 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.arg) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.args) in
    Obj.repr(
# 94 "parser.mly"
                        ( _1::_3 )
# 401 "parser.ml"
               : Ast.args))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.typ) in
    Obj.repr(
# 98 "parser.mly"
                        ( ASTArg(_1, _3) )
# 409 "parser.ml"
               : Ast.arg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 102 "parser.mly"
                    ( ASTEcho(_2) )
# 416 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 103 "parser.mly"
                      ( ASTSet(_2, _3) )
# 424 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.block) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 104 "parser.mly"
                          ( ASTIfBig(_2, _3, _4) )
# 433 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.block) in
    Obj.repr(
# 105 "parser.mly"
                       ( ASTWhile(_2, _3) )
# 441 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 106 "parser.mly"
                        ( ASTCall(_2, _3) )
# 449 "parser.ml"
               : Ast.stat))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'exprp) in
    Obj.repr(
# 110 "parser.mly"
                ( [_1] )
# 456 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exprp) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exprsp) in
    Obj.repr(
# 111 "parser.mly"
                        ( _1::_2 )
# 464 "parser.ml"
               : 'exprsp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 115 "parser.mly"
                        ( ASTExpr(_1) )
# 471 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : string) in
    Obj.repr(
# 116 "parser.mly"
                        ( ASTAdr(_3) )
# 478 "parser.ml"
               : 'exprp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 120 "parser.mly"
                   ( ASTNum(_1) )
# 485 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 121 "parser.mly"
                    ( ASTId(_1) )
# 492 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 122 "parser.mly"
                               ( ASTIf(_3, _4, _5) )
# 501 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 123 "parser.mly"
                             ( ASTAnd(_3, _4) )
# 509 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    Obj.repr(
# 124 "parser.mly"
                            ( ASTOr(_3, _4) )
# 517 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : Ast.exprs) in
    Obj.repr(
# 125 "parser.mly"
                           ( ASTApp(_2, _3) )
# 525 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Ast.args) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 126 "parser.mly"
                           ( ASTLambda(_2, _4) )
# 533 "parser.ml"
               : Ast.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.expr) in
    Obj.repr(
# 130 "parser.mly"
               ( [_1] )
# 540 "parser.ml"
               : Ast.exprs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exprs) in
    Obj.repr(
# 131 "parser.mly"
                  ( _1::_2 )
# 548 "parser.ml"
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
