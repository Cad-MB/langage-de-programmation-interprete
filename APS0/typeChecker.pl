g0(L) :-
	subset(
	[(true,bool),
	(false,bool),
	(not,typeFunc([bool],bool)),
	(eq,typeFunc([int,int],bool)),
	(lt,typeFunc([int,int],bool)),
	(and,typeFunc([bool,bool],bool)),
	(add,typeFunc([int,int],int)),
	(sub,typeFunc([int,int],int)),
	(mul,typeFunc([int,int],int)),
	(div,typeFunc([int,int],int))],
	L).

/*assoc(X,[(X,V)|_],V).
assoc(X,[_|XS],V) :- assoc(X,XS,V).*/

assoc(X,G,V) :-
	subset([(X,V)|[]],G).

get_type([],[]).
get_type([A|ARGS],[T|TYPES]) :-
	typeExpr([],A,T),
	get_type(ARGS,TYPES).

get_typeArgs([],[]).
get_typeArgs([(_,T)|ARGS],[T|RES]) :-
	get_typeArgs(ARGS,RES).
		
checkArgs(_,[],[]).
checkArgs(G,[ARG|ARGS],[ARGTYPE|ARGSTYPE]) :-
	typeExpr(G,ARG,ARGTYPE),
	checkArgs(G,ARGS,ARGSTYPE).


	
/*prog*/
typeProg([],prog(X),void) :- 
	g0(G),
	typeCmds(G,X,void).


/*end()*/
typeCmds(_,[],void).

typeCmds(G,[stat(X)|Y],void) :-
	typeStat(G,X,void),
	typeCmds(G,Y,void).

/*Def*/
typeCmds(G,[def(X)|Y],void) :-
	typeDef(G,X,CB),
	typeCmds(CB,Y,void).

/*echo*/
typeStat(G,echo(X),void) :-
	typeExpr(G,X,int).	



/*const*/
typeDef(G,const(X,T,E),[(X,T)|G]) :-
	typeExpr(G,E,T).
	
/*Fun*/
typeDef(G,fun(ID,T,ARGS,BODY),CB):-
	append(ARGS,G,CT),
	typeExpr(CT,BODY,T),
	get_typeArgs(ARGS,RES),
	CB=[(ID,typeFunc(RES,T))|G].
	
/*funRec*/
typeDef(G,funRec(ID,T,ARGS,BODY),CB):-
	get_typeArgs(ARGS,RES),
	append(ARGS,G,CT),
	CTT = [(ID,typeFunc(RES,T))|CT],
	typeExpr(CTT,BODY,T),
	CB=[(ID,typeFunc(RES,T))|G].


/*Expressions*/

/*true*/
typeExpr(_,true,bool).

/*false*/
typeExpr(_,false,bool).

/*num*/
typeExpr(_,num(X),int) :-
 	integer(X).
 	
/*ident*/
typeExpr(G,id(X),T) :-
	assoc(X,G,T).

/*if*/
typeExpr(G,if(COND,E1,E2),T) :-
	typeExpr(G,COND,bool),
	typeExpr(G,E1,T),
	typeExpr(G,E2,T).

/*app*/
typeExpr(G,app(id(F),ARGS),TF) :-
	assoc(F,G,typeFunc(ARGSTYPE,TF)),
	checkArgs(G,ARGS,ARGSTYPE).
		
/*typeExpr(G,app(abs(ARGSTYPE,BODY),ARGS),TF) :-
	get_typeArgs(ARGSTYPE,RES),
	checkArgs(G,ARGS,RES),
	append(ARGSTYPE,G,CB),
	typeExpr(CB,BODY,TF).*/
	
typeExpr(G,app(app(X,Y),ARGS),TR) :-
	get_type(ARGS,LT),
	typeExpr(G,app(X,Y),typeFunc(LT,TR)).
				
/*abs*/
typeExpr(G,abs(ARGS,BODY),typeFunc(TL,TF)) :-
	get_typeArgs(ARGS,TL),
	append(ARGS,G,CB),
	typeExpr(CB,BODY,TF).
	
/*opérations entières */
/*typeExpr(G,add(X,Y),int) :-
	typeExpr(G,X,int),
	typeExpr(G,Y,int).

typeExpr(G,sub(X,Y),int) :-
	typeExpr(G,X,int),
	typeExpr(G,Y,int).

typeExpr(G,mul(X,Y),int) :-
	typeExpr(G,X,int),
	typeExpr(G,Y,int).

typeExpr(G,div(X,Y),int) :-
	typeExpr(G,X,int),
	typeExpr(G,Y,int).*/

/*opérations booléennes*/
/*typeExpr(G,and(X,Y),bool) :-
	typeExpr(G,X,bool),
	typeExpr(G,Y,bool).

typeExpr(G,or(X,Y),bool) :-
	typeExpr(G,X,bool),
	typeExpr(G,Y,bool).

typeExpr(G,eq(X,Y),bool) :-
	typeExpr(G,X,int),
	typeExpr(G,Y,int).

typeExpr(G,lt(X,Y),bool) :-
	typeExpr(G,X,int),
	typeExpr(G,Y,int).
	
typeExpr(G,not(X),bool) :-
	typeExpr(G,X,bool).*/

main_stdin :-
	read(user_input,T),
	typeProg([],T,R),
	print(R).

/*g0([
	(true,bool),
	(false,bool),
	(not,typeFunc([bool],bool)),
	(eq,typeFunc([int,int],bool)),
	(lt,typeFunc([int,int],bool)),
	(and,typeFunc([bool,bool],bool)),
	(add,typeFunc([int,int],int)),
	(sub,typeFunc([int,int],int)),
	(mul,typeFunc([int,int],int)),
	(div,typeFunc([int,int],int))
	]).*/