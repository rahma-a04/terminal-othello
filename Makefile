
othello:
	OCAMLRUNPARAM=b dune exec bin/main.exe

tests:
	OCAMLRUNPARAM=b dune exec test/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh	