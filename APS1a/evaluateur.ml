open Ast

(*V = Z ⊕ F ⊕ FR*)
type valeur =
  (*Z*)
  | InZ of int
  (*F = Expr × ident∗ × E*)
  | InF of expr * string list * env list
  (*FR = Expr × ident × ident∗ × E*)
  | InFR of expr * string * string list * env list
  (*[Adresse] A*)
  | InA of int
  (*[Fermetures procédurales] P = Cmds × ident∗ × E*)
  | InP of block * string list * env list
    (*[Fermetures procédurales récursives] PR = Cmds × ident × ident∗ × E*)
  | InPR of block * string * string list * env list

(*E = ident → V*)
and env = Couple of string * valeur

(*[Memoire] S = A → Z (fonction partielle)*)
type memoire = Mem of int * valeur ref

(*check si un ident a une valeur associe dans l'env*)
let rec in_env ident v =
  match v with
  | [] -> false
  | Couple (str, _) :: tl ->
      if String.equal str ident then true else in_env ident tl

(*check si une app est une prim1*)
let rec is_prim1 e = match e with ASTId "not" -> true | _ -> false

(*check si une app est une prim2*)
let rec is_prim2 e =
  match e with
  | ASTId ("eq" | "lt" | "add" | "sub" | "mul" | "div") -> true
  | _ -> false

(*checrche la valeur associe a l'ident dans l'env*)
let rec chercher_env ident v =
  match v with
  | Couple (str, v) :: tl ->
      if String.equal str ident then v else chercher_env ident tl
  | _ -> failwith "erreur chercher_env"

let rec in_mem adr l =
  match l with
  | [] -> false
  | Mem (a, _) :: tl -> if a = adr then true else in_mem adr tl

let rec chercher_mem adr l =
  match l with
  | Mem (a, v) :: tl -> if a = adr then v else chercher_mem adr tl
  | _ -> failwith "erreur chercher_mem"

(*injection canonique inZ*)
let get_InZ v = match v with InZ n -> n | _ -> failwith "erreur get_InZ"

(*Fonctions primitives*)
(*bop*)
let prim_1v op v =
  match op with
  | ASTId "not" ->
      if get_InZ v == 0 then InZ 1
      else if get_InZ v == 1 then InZ 0
      else failwith "erreur prim_1v"
  | _ -> failwith "Not a valid Prim1"
(*uop*)

let prim_2v op v1 v2 =
  let inZ_v1 = get_InZ v1 and inZ_v2 = get_InZ v2 in
  match op with
  | ASTId "eq" -> if inZ_v1 == inZ_v2 then InZ 1 else InZ 0
  | ASTId "lt" -> if inZ_v1 < inZ_v2 then InZ 1 else InZ 0
  | ASTId "add" -> InZ (inZ_v1 + inZ_v2)
  | ASTId "sub" -> InZ (inZ_v1 - inZ_v2)
  | ASTId "mul" -> InZ (inZ_v1 * inZ_v2)
  | ASTId "div" -> InZ (inZ_v1 / inZ_v2)
  | _ -> failwith "Not a valid Prim2"

let rec get_args l =
  match l with [] -> [] | ASTArg (str, _) :: tl -> str :: get_args tl

(*Expression*)
let rec calcul_expression expr env mem =
  match expr with
  (*TRUE*)
  | ASTId "true" -> InZ 1
  (*FALSE*)
  | ASTId "false" -> InZ 0
  (*NUM*)
  | ASTNum n -> InZ n
  (*ID*)
  | ASTId id ->
      if in_env id env then
        match chercher_env id env with
        | InA a ->
            if in_mem a mem then !(chercher_mem a mem)
            else failwith "adresse n'existe pas"
        | v -> v
      else failwith (id ^ " variable absente de l'environnement")
  (*AND*)
  | ASTAnd (e1, e2) ->
      if calcul_expression e1 env mem = InZ 0 then InZ 0
      else calcul_expression e2 env mem
  (*OR*)
  | ASTOr (e1, e2) ->
      if calcul_expression e1 env mem = InZ 1 then InZ 1
      else calcul_expression e2 env mem
  (*IF*)
  | ASTIf (e1, e2, e3) ->
      if calcul_expression e1 env mem = InZ 1 then calcul_expression e2 env mem
      else calcul_expression e3 env mem
  (*ABS*)
  | ASTLambda (al, e) -> InF (e, get_args al, env)
  (*APP*)
  | ASTApp (e, el) -> (
      if is_prim1 e then
        (*PRIM1*)
        match el with
        | [] -> failwith "Missing argument for unary operator"
        | [ e1 ] -> prim_1v e (calcul_expression e1 env mem)
        | _ -> failwith "Too many arguments for unary operator"
      else if is_prim2 e then
        (*PRIM2*)
        match el with
        | [] -> failwith "Missing arguments for binary operator"
        | [ e1; e2 ] ->
            prim_2v e
              (calcul_expression e1 env mem)
              (calcul_expression e2 env mem)
        | _ -> failwith "Too many arguments for binary operator"
      else
        match calcul_expression e env mem with
        (*APP*)
        | InF (e2, args, env2) ->
            let new_env = env2 @ assoc_arg_val args el env mem in
            calcul_expression e2 new_env mem
        (*APPR*)
        | InFR (e2, f, args, env2) ->
            let new_env =
              env2
              @ assoc_arg_val args el env mem
              @ [ Couple (f, InFR (e2, f, args, env2)) ]
            in
            calcul_expression e2 new_env mem
        | _ -> failwith "erreur calcul_expression ASTApp")

(*sert au nouvelle environnement*)
and assoc_arg_val args el env mem =
  match (args, el) with
  | [], [] -> []
  | arg :: atl, e :: etl ->
      Couple (arg, calcul_expression e env mem) :: assoc_arg_val atl etl env mem
  | _ -> failwith "erreur assoc_arg_val"

(*Definition*)
(*allocation*)

let alloc_indice = ref 0

let alloc mem =
  let res = (!alloc_indice, Mem (!alloc_indice, ref (InZ (-1))) :: mem) in
  alloc_indice := !alloc_indice + 1;
  res

let rec calcul_def expr env mem =
  match expr with
  (*CONST*)
  | ASTConst (str, _, e) ->
      (Couple (str, calcul_expression e env mem) :: env, mem)
  (*FUN*)
  | ASTFun (str, _, args, e) ->
      (Couple (str, InF (e, get_args args, env)) :: env, mem)
  (*FUNREC*)
  | ASTFunRec (str, _, args, e) ->
      (Couple (str, InFR (e, str, get_args args, env)) :: env, mem)
  (*VAR*)
  | ASTVar (str, _) ->
      let adr, mem_prime = alloc mem in
      (Couple (str, InA adr) :: env, mem_prime)
  (*PROC*)
  | ASTProc (str, args, bk) ->
      (Couple (str, InP (bk, get_args args, env)) :: env, mem)
  (*PROCREC*)
  | ASTProcRec (str, args, bk) ->
      (Couple (str, InPR (bk, str, get_args args, env)) :: env, mem)

(*Suites de commandes*)
let rec calcul_cmds cmds env mem flux =
  match cmds with
  (*DEFS*)
  | ASTDefEtc (def, tl) ->
      let new_env, new_mem = calcul_def def env mem in
      calcul_cmds tl new_env new_mem flux
  (*STAT*)
  | ASTStatEtc (stat, tl) ->
      let new_mem, new_flux = calcul_instr stat env mem flux in
      calcul_cmds tl env new_mem new_flux
  (*END*)
  | ASTStat stat -> calcul_instr stat env mem flux
(*| _ -> failwith "erreur calcul_cmds"*)

and calcul_block cmds env mem flux =
  match cmds with ASTBlock cs -> calcul_cmds cs env mem flux

(*Instruction*)
and calcul_instr stat env mem flux =
  match stat with
  (*ECHO*)
  | ASTEcho e -> (mem, get_InZ (calcul_expression e env mem) :: flux)
  | ASTSet (x, e) ->
      (* Si l'AST est un ASTSet, on entre dans cette branche *)
      if in_env x env then
        (* On vérifie si la variable x existe dans l'environnement *)
        match chercher_env x env with
        (* On cherche l'adresse mémoire de la variable x dans l'environnement *)
        | InA adr ->
            (* Si l'adresse de x est trouvée, on entre dans cette branche *)
            let v = calcul_expression e env mem in
            (* On calcule la valeur de l'expression à droite de l'opérateur d'affectation *)
            if in_mem adr mem then (
              (* On vérifie si l'adresse de x existe dans la mémoire *)
              let v_pre = chercher_mem adr mem in
              (* On cherche la valeur précédente de x dans la mémoire *)
              v_pre := v;
              (* On modifie la valeur précédente de x dans la mémoire avec la nouvelle valeur v *)
              (mem, flux)
              (* On retourne la mémoire et le flux inchangés *))
            else failwith "adresse n'existe pas"
            (* Si l'adresse de x n'existe pas dans la mémoire, on lève une exception *)
        | _ -> failwith "mauvaise valeur"
        (* Si x n'est pas une adresse mémoire, on lève une exception *)
      else failwith "ident n'existe pas"
        (* Si x n'existe pas dans l'environnement, on lève une exception *)
  | ASTWhile (e, b) ->
      if calcul_expression e env mem = InZ 0 then (mem, flux)
      else
        let mem1, flux1 = calcul_block b env mem flux in
        calcul_instr stat env mem1 flux1
  | ASTIfBig (e, b1, b2) ->
      if calcul_expression e env mem = InZ 1 then calcul_block b1 env mem flux
      else calcul_block b2 env mem flux
  (*| ASTCall (e, el) -> (
      match calcul_expression e env mem with
      | InP (bk, args, env2) ->
          let new_env = env2 @ assoc_arg_val args el env mem in
          calcul_block bk new_env mem flux
      | InPR (bk, p, args, env2) ->
          let new_env =
            env2
            @ assoc_arg_val args el env mem
            @ [ Couple (p, InPR (bk, p, args, env2)) ]
          in
          calcul_block bk new_env mem flux
      | _ -> failwith "erreur calcul_expression ASTApp")*)
  | ASTCall (x, el) ->
      if in_env x env then
        match chercher_env x env with
        | InP (bk, xl, env2) ->
            let env3 = env2 @ assoc_arg_val xl el env mem in
            calcul_block bk env3 mem flux
        | InPR (bk, x, xl, env2) ->
            let env3 =
              env2
              @ assoc_arg_val xl el env mem
              @ [ Couple (x, InPR (bk, x, xl, env2)) ]
            in
            calcul_block bk env3 mem flux
        | _ -> failwith "erreur calcul_expression ASTApp"
      else failwith "ident n'existe pas"

let print_val value = match value with n -> Printf.printf "%d\n" n

let rec print_output (mem, output) =
  List.iter (function x -> print_val x) (List.rev output)

(*Programmes*)
let calcul_prog prog =
  match prog with ASTProg bk -> print_output (calcul_block bk [] [] [])
;;

let fname = Sys.argv.(1) in
let ic = open_in fname in
try
  let lexbuf = Lexing.from_channel ic in
  let p = Parser.prog Lexer.token lexbuf in
  calcul_prog p;
  print_string ".\n"
with Lexer.Eof -> exit 0
