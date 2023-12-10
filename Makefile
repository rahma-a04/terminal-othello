.PHONY: test check

othello:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc:
	dune build @doc

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

opendoc: doc
	@bash opendoc.sh	

zip:
	rm -f othello.zip
	zip -r othello.zip . -x@exclude.lst