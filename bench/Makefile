TESTS = \
  bench bench_monad bench_ipc bench_ipc_monad bench2


TESTS_OPT = $(TESTS:%=%.opt)

# OCAMLC = ocamlfind ocamlc -linkpkg -I ../src -w +A -rectypes -thread -package unix,threads,core
OCAMLOPT = ocamlfind ocamlopt -O3 -inlining-report -linkpkg -I ../src -w +A -rectypes -thread -package unix,threads,core
# DECODER = ../decoder/rosetta

all: $(TESTS_OPT)

%.opt: %.ml ../src/FuSe.cmxa
	$(OCAMLOPT) -o $@ FuSe.cmxa $<

clean:
	rm -f *~ *.cmo *.cmi *.cmx *.o *.t $(TESTS) $(TESTS_OPT)

.PHONY: clean html

