build:
	ocamlopt -c expr.ml
	ocamlopt -c pushdown.ml
	ocamlopt -c main.ml
	ocamlopt -o main expr.cmx pushdown.cmx

.PHONY: clean

clean:
	rm -f *.o *.cmx *.cmi main
