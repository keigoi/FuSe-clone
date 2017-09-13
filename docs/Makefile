
NULL =

SOURCES = \
  branches.tex \
  initiation.tex \
  introduction.tex \
  io.tex \
  iterators.tex \
  linearity.tex \
  low_level.tex \
  macros.tex \
  main.tex \
  notation.tex \
  polymorphism.tex \
  recursion.tex \
  services.tex \
  subtyping.tex \
  $(NULL)

all:

FuSe.pdf: $(SOURCES)
	pdflatex main
	bibtex main
	pdflatex main
	pdflatex main
	cp main.pdf $@

clean:
	rm -f *.pdf *.aux *.log *.out *.toc *.bbl *.blg
