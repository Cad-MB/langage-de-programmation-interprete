open Ast;;

(*V = Z ⊕ F ⊕ FR*)
type valeur = 
			(*Z*)
			InZ of int 
			(*F = Expr × ident∗ × E*)
			| InF of expr * string list * env list 
			(*FR = Expr × ident × ident∗ × E*)
			| InFR of expr * string * string list * env list 
(*E = ident → V*)
and env = Couple of string * valeur 

(*check si un ident a une valeur associe dans l'env*)
let rec in_env ident v =
	match v with
	  [] -> false
	| Couple(str, _)::tl ->  if (String.equal str ident) then true else (in_env ident tl)

(*check si une app est une prim1*)
let rec is_prim1 e =
	match e with
	| ASTId("not") -> true
	| _ -> false

(*check si une app est une prim2*)
let rec is_prim2 e =
	match e with
	| ASTId("eq"|"lt"|"add"|"sub"|"mul"|"div") -> true
	| _ -> false

(*checrche la valeur associe a l'ident dans l'env*)
let rec chercher_env ident v =
	match v with
	| Couple(str, v)::tl ->  if (String.equal str ident) then v else (chercher_env ident tl)
	| _ -> failwith "erreur chercher_env"


(*injection canonique inZ*)
let get_InZ v = 
	match v with
	| InZ(n) -> n
	| _ -> failwith "erreur get_InZ"

(*Fonctions primitives*)
  (*bop*)
let prim_1v op v =
	match op with
	| ASTId("not") -> if (get_InZ v) == 0 then InZ(1)
			else if (get_InZ v) == 1 then InZ(0)
			else failwith "erreur prim_1v"
  (*uop*)
let prim_2v op v1 v2 =
	let inZ_v1 = (get_InZ v1) 
	and inZ_v2 = (get_InZ v2)
	in
	match op with
	| ASTId("eq") -> if (inZ_v1 == inZ_v2) then InZ(1) else InZ(0)
	| ASTId("lt") -> if (inZ_v1 < inZ_v2) then InZ(1) else InZ(0)
	| ASTId("add") -> InZ(inZ_v1 + inZ_v2)
	| ASTId("sub") -> InZ(inZ_v1 - inZ_v2)
	| ASTId("mul") -> InZ(inZ_v1 * inZ_v2)
	| ASTId("div") -> InZ(inZ_v1 / inZ_v2)

let rec get_args l = 
	match l with
	 [] -> []
	|ASTArg(str, _)::tl -> str::(get_args tl)

(*Expression*)
let rec calcul_expression expr env mem =
	match expr with
	(*TRUE*)
	|ASTId("TRUE") -> InZ(1)
	(*FALSE*)
	|ASTId("FALSE") -> InZ(0)
	(*NUM*)
	|ASTNum(n) -> InZ(n)    
	(*ID*)
	| ASTId(id) -> if in_env id env then chercher_env id env else failwith (id^" variable absente de l'environnement")
	(*AND*)
	|ASTAnd(e1, e2) -> if (calcul_expression e1 env mem) = InZ(0) 
							then InZ(0) 
							else (calcul_expression e2 env mem) 
	(*OR*)
	|ASTOr(e1, e2) -> if (calcul_expression e1 env mem) = InZ(1) 
							then InZ(1) 
							else (calcul_expression e2 env mem) 
	(*IF*)
	|ASTIf(e1, e2, e3) -> if (calcul_expression e1 env mem) = InZ(1)
							then (calcul_expression e2 env mem)
							else (calcul_expression e3 env mem)
	(*ABS*)
	|ASTLambda(al, e) -> InF(e, get_args(al), env)
	(*APP*)
	|ASTApp(e, el) -> 
					if is_prim1 e then  (*PRIM1*)
						(match el with
						| [] -> failwith "Missing argument for unary operator"
						| e1 :: [] -> prim_1v e (calcul_expression e1 env mem)
						| _ -> failwith "Too many arguments for unary operator")
					else if is_prim2 e then (*PRIM2*)
						(match el with
						| [] -> failwith "Missing arguments for binary operator"
						| e1 :: e2 :: [] -> prim_2v e (calcul_expression e1 env mem) (calcul_expression e2 env mem)
						| _ -> failwith "Too many arguments for binary operator")

					else
						match (calcul_expression e env mem) with
						(*APP*)
						| InF(e2, args, env2) ->
							let new_env = env2@(assoc_arg_val args el env mem) in
							calcul_expression e2 new_env mem
						(*APPR*)
						| InFR(e2, f, args, env2) ->
							let new_env = env2@(assoc_arg_val args el env mem)@
							[Couple(f, InFR(e2, f, args, env2))] in
							calcul_expression e2 new_env mem
						| _ -> failwith "erreur calcul_expression ASTApp"
(*sert au nouvelle environnement*)
and assoc_arg_val args el env mem = 
	match args,el with
	|[],[] -> []
	|arg::atl, e::etl -> Couple(arg, calcul_expression e env mem)::(assoc_arg_val atl etl env mem) 
	|_ -> failwith "erreur assoc_arg_val"

(*Definition*)
let rec calcul_def expr env mem =
	match expr with
	(*CONST*)
	| ASTConst(str, _, e) -> (Couple(str, (calcul_expression e env mem))::env, mem)
	(*FUN*)
	| ASTFun(str, _, args, e) -> (Couple(str, InF(e, (get_args args), env))::env, mem)
	(*FUNREC*)
	| ASTFunRec(str, _, args, e) -> (Couple(str, InFR(e, str, (get_args args), env))::env, mem)

(*Suites de commandes*)
let rec calcul_cmds cmds env mem flux = 
	match cmds with
	(*END*)
	| ASTStat(stat) -> calcul_instr stat env mem flux
	(*DEFS*)
	| ASTDef(def, tl) -> let (new_env, new_mem) = calcul_def def env mem in
						calcul_cmds tl new_env new_mem flux

(*Instruction*)
and calcul_instr stat env mem flux =
	match stat with
	(*ECHO*)
	| ASTEcho(e) -> (mem, get_InZ(calcul_expression e env mem)::flux)
let print_val value =
	match value with
	|	n -> Printf.printf "%d\n" n
	
let rec print_output (mem, output) =
	List.iter (function x -> print_val x) (List.rev output)

(*Programmes*)
let calcul_prog prog =
	match prog with
		| ASTProg(cmds) -> print_output (calcul_cmds cmds [] [] [])
;;

let fname = Sys.argv.(1) in
	let ic = open_in fname in
		try
			let lexbuf = Lexing.from_channel ic in
			let p = Parser.prog Lexer.token lexbuf in
				calcul_prog p;
				print_string ".\n"
		with Lexer.Eof ->
			exit 0