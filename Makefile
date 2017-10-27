DEPENDS = extlib ounit
MAIN = main
# Topologically ordered dependencies.
# TODO(richardwu): Figure out a better way to do this (i.e. ocamldep )
OBJECTS = table.cmo columnsPerTable.cmo columns.cmo cnf.cmo expr.cmo
INTERFACES = $(OBJECTS:%.cmo=%.cmi)
TESTSRCS = cnf_test.ml props_test.ml
TESTS = $(TESTSRCS:%.ml=%)
TESTPKGS = oUnit
CC = ocamlc
OCAMLF = ocamlfind


.PHONY: setup
setup: $(DEPENDS)
$(DEPENDS):
	opam install $@

.PHONY: build
build: $(MAIN)

.PHONY: test
test: # Run all tests
test: $(TESTS)
	$(foreach testfile, $^, ./$(testfile);)

%.cmi: %.mli
	$(CC) -c $<

%.cmo: %.ml
	$(CC) -c $<

$(OBJECTS): $(INTERFACES)

%_test: %_test.ml $(OBJECTS)
	$(OCAMLF) $(CC) -o $@ -package $(TESTPKGS) -linkpkg -g $(OBJECTS) $<

$(MAIN): $(OBJECTS)
	$(CC) -o $(MAIN) $(OBJECTS)

clean:
	rm -f $(OBJECTS) $(INTERFACES) $(TESTDEPS) $(TESTS) $(MAIN)
