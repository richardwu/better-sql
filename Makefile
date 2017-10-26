DEPENDS = extlib
MAIN = main
# Topologically ordered dependencies.
# TODO(richardwu): Figure out a better way to do this.
OBJECTS = table.cmo columnsPerTable.cmo columns.cmo expr.cmo props_test.cmo
INTERFACES = $(OBJECTS:%.cmo=%.cmi)
CC = ocamlc


.PHONY: setup
setup: $(DEPENDS)
$(DEPENDS):
	opam install $@

.PHONY: build
build: $(MAIN)

%.cmi: %.mli
	$(CC) -c $<

%.cmo: %.ml
	$(CC) -c $<

$(MAIN): $(INTERFACES) $(OBJECTS)
	$(CC) -o $(MAIN) $(OBJECTS)

clean:
	rm -f $(OBJECTS) $(INTERFACES) $(MAIN)
