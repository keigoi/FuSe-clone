NULL =

TESTS = \
  bench bench_monad


TESTS_OPT = $(TESTS:%=%.opt)

OCAMLC = ocamlfind ocamlc -linkpkg -I ../src -w +A -rectypes -thread -package unix,threads,core
OCAMLOPT = ocamlfind ocamlopt -linkpkg -I ../src -w +A -rectypes -thread -package unix,threads,core
DECODER = ../decoder/rosetta

all: $(TESTS) $(TESTS_OPT)

all_t: $(TESTS:%=%.t)

all_st: $(TESTS:%=%.st)

%.t: %.ml
	@$(OCAMLC) -i $<

%.st: %.ml
	@$(OCAMLC) -i $< | $(DECODER)

%: %.ml ../src/FuSe.cma
	$(OCAMLC) -o $@ FuSe.cma $<

%.opt: %.ml ../src/FuSe.cmxa
	$(OCAMLOPT) -o $@ FuSe.cmxa $<

clean:
	rm -f *~ *.cmo *.cmi *.cmx *.o *.t $(TESTS) $(TESTS_OPT)

rename: $(TESTS:test-%=test.%.ml) $(FAILING_TESTS:fail-%=fail.%.ml)

test.%: test-%
	svn mv $< $@

fail.%: fail-%
	svn mv $< $@

.PHONY: clean html

