# ==============================================================================
# === UPMC/master/info/4I506 -- Janvier 2016/2017/2018                        ==
# === SU/FSI/master/info/MU4IN503 -- Janvier 2020/2021                        ==
# === Analyse des programmes et sémantiques                                   ==
# ==============================================================================
# === S-expressions Syntaxe ML                                                ==
# === Fichier: Makefile                                                       ==
# ==============================================================================

LEX_ML = ocamllex
YACC_ML = ocamlyacc
OCAMLC = ocamlc

prologTerm: parser prologTerm.ml
	$(OCAMLC) -o prologTerm ast.cmo lexer.cmo parser.cmo prologTerm.ml

evaluateur: parser evaluateur.ml
	$(OCAMLC) -o evaluateur ast.cmo lexer.cmo parser.cmo evaluateur.ml

parser: ast.ml lexer.mll parser.mly
	$(OCAMLC) -c ast.ml
	$(LEX_ML) -o lexer.ml lexer.mll
	$(YACC_ML) -b parser parser.mly
	$(OCAMLC) -c parser.mli
	$(OCAMLC) -c lexer.ml
	$(OCAMLC) -c parser.ml

printer: parser printer.ml
	$(OCAMLC) -o printer ast.cmo lexer.cmo parser.cmo printer.ml

clean:
	rm -f *.cmo
	rm -f *.cmi
	rm -f prologTerm
	rm -f printer
	rm -f lexer.ml
	rm -f parser.mli
	rm -f parser.ml
	rm -f evaluateur
	rm *~
