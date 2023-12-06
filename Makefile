
othello:
	OCAMLRUNPARAM=b dune exec bin/main.exe

tests:
	OCAMLRUNPARAM=b dune exec test/main.exe