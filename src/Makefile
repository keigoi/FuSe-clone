
NULL =

SOURCES = SessionBase.ml Session.mli SessionIPC.ml Service.mli Session.ml Service.ml

OCAMLC = ocamlfind ocamlc -w +A-20 -rectypes -thread -package core
OCAMLOPT = ocamlfind ocamlopt -O3 -inlining-report -w +A-20 -rectypes -thread -package core

DEST = padovani@pianeta.di.unito.it:public_html/Software/FuSe/

SYNC_FILES = $(wildcard html/*)

all: FuSe.cma FuSe.cmxa

FuSe.cma: $(SOURCES)
	$(OCAMLC) -a -o $@ $^

FuSe.cmxa: $(SOURCES)
	$(OCAMLOPT) -a -o $@ $^

sync: $(SYNC_FILES)
	scp $^ $(DEST)

clean:
	rm -f *~ *.cma *.cmxa *.cmo *.cmi *.cmx *.o *.t *.a

.PHONY: clean html

