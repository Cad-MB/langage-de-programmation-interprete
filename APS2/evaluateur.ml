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
  | InB of int * int

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
  | _ -> failwith "prim_1v non vqlide"
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
  | _ -> failwith "prim_2v non vqlide"

let rec get_args l =
  match l with [] -> [] | ASTArg (str, _) :: tl -> str :: get_args tl

let rec get_argsp l =
  match l with
  | [] -> []
  | ASTArgp (str, _) :: tl -> str :: get_argsp tl
  | ASTArgpVar (str, _) :: tl -> str :: get_argsp tl

(*Expression*)
let rec calcul_expression expr env mem =
  match expr with
  (*TRUE*)
  | ASTId "true" -> (InZ 1, mem)
  (*FALSE*)
  | ASTId "false" -> (InZ 0, mem)
  (*NUM*)
  | ASTNum n -> (InZ n, mem)
  (*ID*)
  | ASTId id ->
      if in_env id env then
        match chercher_env id env with
        | InA a ->
            if in_mem a mem then (!(chercher_mem a mem), mem)
            else failwith "adresse n'existe pas"
        | v -> (v, mem)
      else failwith (id ^ " variable absente de l'environnement")
  (*AND*)
  | ASTAnd (e1, e2) -> let (v, mem1) = calcul_expression e1 env mem in
                            if v == InZ 0 
                            then (InZ(0), mem1) 
                            else calcul_expression e2 env mem1
  (*OR*)
  | ASTOr (e1, e2) -> let (v, mem1) = calcul_expression e1 env mem in
                            if v == InZ 1 
                            then (InZ(1), mem1) 
                            else calcul_expression e2 env mem1
  (*IF*)
  | ASTIf (e1, e2, e3) -> let (v, mem1) = calcul_expression e1 env mem in
                            if v = InZ 1
                            then calcul_expression e2 env mem1
                            else calcul_expression e3 env mem1
  (*ABS*)
  | ASTLambda (al, e) -> InF (e, get_args al, env)
  (*APP*)
  | ASTApp (e, el) -> (
      if is_prim1 e then
        (*PRIM1*)
        match el with
        | [] -> failwith "arguments manquant"
        | [ e1 ] -> prim_1v e (calcul_expression e1 env mem)
        | _ -> failwith "Trop d'arguments"
      else if is_prim2 e then
        (*PRIM2*)
        match el with
        | [] -> failwith "arguments manquant"
        | [ e1; e2 ] ->
            prim_2v e
              (calcul_expression e1 env mem)
              (calcul_expression e2 env mem)
        | _ -> failwith "Trop d'arguments"
      else
        let (f, env_prim) = (calcul_expression e env mem) in
                    match f with
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
(*len*)
|ASTLen e -> let (v, new_mem) = calcul_expression e env mem in
        begin
        match v with
        | InB (a,n) -> (InZ n , new_mem)
        | _ -> failwith "erreur len"
        end
(*nth*)
|ASTNth (e1, e2) -> 
        let (v1, mem1) = calcul_expression e1 env mem in
            let (v2, mem2) = calcul_expression e2 env mem1 in
                begin
                    match v2 with
                    | InZ i -> (
                        match v1 with
                        | InB(a,n) -> (
                            if (i < n) 
                            then (!(chercher_mem (a+i) mem2), mem2)
                            else failwith "ASTNth stack overflow"; )
                        | InZ i -> failwith "erreur ASTNth inZ : pas un tableau" ; 
                        | _ -> failwith "erreur nth : pas un tableau"; 
                        )
                    | _ -> failwith "erreur nth"
                end    
(*alloc*)
|ASTAlloc e -> let (v, new_mem) = calcul_expression e env mem in
        begin
        match v with
        | InZ n -> let (a, mem_prim) = allocn new_mem n in
                        (InB(a,n), mem_prim)
        | _ -> failwith "erreur alloc : pas un tableau"
        end 
(*vset*)
|ASTVset(e1, e2, e3) -> 
        let (v1, mem1) = calcul_expression e1 env mem in
        let (v2, mem2) = calcul_expression e2 env mem1 in
        let (v3, mem3) = calcul_expression e3 env mem2 in
            begin
                match v2 with
                | InZ i -> (
                    match v1 with
                    | InB(a,n) ->  if (i < n) 
                                    then (
                                        (chercher_mem (a+i) mem3) := v3;
                                        (InB(a,n), mem3)
                                    )
                                    else failwith "vset"; 
                    | _ -> failwith "erreur vset : pas un tableau"; 
                  )
                | _ -> failwith "erreur vset" 
            end     

(*sert au nouvelle environnement*)
and assoc_arg_val args el env mem =
  match (args, el) with
  | [], [] -> []
  | arg :: atl, e :: etl ->
      Couple (arg, calcul_expression e env mem) :: assoc_arg_val atl etl env mem
  | _ -> failwith "erreur assoc_arg_val"

(*aps1a*)
let calcul_exprp ep env mem =
  match ep with
  | ASTExpr e -> calcul_expression e env mem
  | ASTAdr x ->
      if in_env x env then
        match chercher_env x env with InA a -> InA a | v -> v
      else failwith "ident n'existe pas"

let rec assoc_arg_val_p args el env mem =
  match (args, el) with
  | [], [] -> []
  |arg::atl, e::etl -> let (v, new_mem) = calcul_exprp e env mem in 
                        Couple(arg, v) :: assoc_arg_val_p atl etl env mem 
    |_ -> failwith "erreur assoc_arg_val_p"

(*Definition*)
(*allocation*)

let alloc_indice = ref 0

let alloc mem = 
    let res = (!alloc_indice, Mem(!alloc_indice, ref (InZ(-1)))::mem) in
    alloc_indice := !alloc_indice + 1;
    res

let allocn mem n = 
    if (n <= 0) then failwith "argument invalide pour l'alloc" 
    else
        let adresse = !alloc_indice in
            let rec add_to_mem new_mem size =
                match size with
                |0 -> new_mem
                |_ -> (
                    let tmp = !alloc_indice in
                    alloc_indice := (!alloc_indice +1);
                    add_to_mem (Mem(tmp, ref (InZ -1 ))::new_mem) (size-1)
                ) 
            in (adresse, (add_to_mem mem n))

let rec calcul_def expr env mem =
  match expr with
  (*CONST*)
  | ASTConst(str, _, e) -> let (v, new_mem) = calcul_expression e env mem in
                            (Couple(str, v)::env, new_mem)
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
      (Couple (str, InP (bk, get_argsp args, env)) :: env, mem)
  (*PROCREC*)
  | ASTProcRec (str, args, bk) ->
      (Couple (str, InPR (bk, str, get_argsp args, env)) :: env, mem)


(* Lvalue *)
let rec calcul_lval lval env mem = 
  match lval with
  | ASTLvId x  -> 
      if in_env x env then 
          match chercher_env x env with
          | InA a -> (InA a, mem)
          | InB(a, n) -> (InB(a,n) , mem)
          | _ -> failwith "ASTLvId"
      else failwith "ident n'existe pas"
  | ASTLvVar(lv, e) -> 
      let (v1, mem1) = calcul_lval lv env mem in
          match v1 with
          | InB(a, n) -> 
            (
              let (v, new_mem) = calcul_expression e env mem1 in
              match v with
              | InZ x -> 
                (
                  let ad = a + x in
                      match !(chercher_mem ad mem) with
                      | InB(adr, n) ->  (InB(adr, n), new_mem)
                      | _ -> (InA ad, new_mem)
                )
              | _ -> failwith "pas un entier"
            )
          | _ -> failwith "pas un bloc"

(*Suites de commandes*)
let rec calcul_cmds cmds env mem flux =
  match cmds with
  (*DEFS*)
  | ASTDef (def, tl) ->
      let new_env, new_mem = calcul_def def env mem in
      calcul_cmds tl new_env new_mem flux
  (*STAT*)
  | ASTStat (stat, tl) ->
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
  | ASTEcho e -> let (v, new_mem) = calcul_expression e env mem in
                  (new_mem, get_InZ v::flux)
    | ASTSet(lv, e) ->  let (v, mem1) = calcul_expression e env mem in
                        let (adr, mem2) = calcul_lval lv env mem1 in
                        (
                            match adr with
                            | InA a -> (
                                chercher_mem a mem2 := v;
                                (mem2, flux)
                                )
                            |_ -> failwith "erreur calcul_expression ASTSet"
                        )
    | ASTWhile(e, b) -> let (v, mem1) = calcul_expression e env mem in
                        if v = InZ 0
                        then (mem1,flux)
                        else 
                        begin
                            let (mem2,flux1) = calcul_block b env mem1 flux in
                            calcul_instr stat env mem2 flux1
                        end
    | ASTIfBig(e, b1, b2) -> let (v, new_mem) = calcul_expression e env mem in
                            if v = InZ 1
                            then calcul_block b1 env new_mem flux
                            else calcul_block b2 env new_mem flux 
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
            let env3 = env2 @ assoc_arg_val_p xl el env mem in
            calcul_block bk env3 mem flux
        | InPR (bk, x, xl, env2) ->
            let env3 =
              env2
              @ assoc_arg_val_p xl el env mem
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
