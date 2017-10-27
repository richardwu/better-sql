DEPENDS = extlib
MAIN = main
# Topologically ordered dependencies.
# TODO(richardwu): Figure out a better way to do this (i.e. ocamldep )
OBJECTS = table.cmo columnsPerTable.cmo columns.cmo cnf.cmo expr.cmo
INTERFACES = $(OBJECTS:%.cmo=%.cmi)
TESTDEPS = cnf_test.cmo props_test.cmo
TESTS = $(TESTDEPS:%.cmo=%)
CC = ocamlc


.PHONY: setup
setup: $(DEPENDS)
$(DEPENDS):
	opam install $@

.PHONY: build
build: $(MAIN)

.PHONY: buildtests
buildtests: $(TESTS)

%.cmi: %.mli
	$(CC) -c $<

%.cmo: %.ml
	$(CC) -c $<

$(OBJECTS): $(INTERFACES)

%_test: %_test.cmo
	$(CC) -o $@ $(OBJECTS) $<

$(MAIN): $(OBJECTS)
	$(CC) -o $(MAIN) $(OBJECTS)

clean:
	rm -f $(OBJECTS) $(INTERFACES) $(TESTDEPS) $(TESTS) $(MAIN)
