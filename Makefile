
NAME = FuSe
VERSION = 0.7
BASE = $(NAME)-$(VERSION)

NULL =

DIRS = src decoder examples html docs

ARCHIVE = \
  Makefile \
  $(wildcard src/*.ml) \
  $(wildcard src/*.mli) \
  src/Makefile \
  $(wildcard decoder/*.ml) \
  $(wildcard decoder/*.mli) \
  $(wildcard decoder/*.mly) \
  $(wildcard decoder/*.mll) \
  decoder/Makefile \
  $(wildcard examples/*.ml) \
  examples/Makefile \
  $(wildcard html/*.html) \
  $(wildcard html/*.css) \
  html/Makefile \
  docs/Makefile \
  docs/FuSe.pdf \
  LICENSE \
  LICENSE.ml \
  README.md \
  CONTRIBUTORS \
  $(NULL)

SYNC_HOME_FILES = \
  FuSe.css \
  FuSe.html \
  $(NULL)

SYNC_FILES = \
  $(SYNC_HOME_FILES) \
  $(BASE).tar.gz \
  $(wildcard html/*.html) \
  $(wildcard html/*.css) \
  docs/FuSe.pdf \
  $(NULL)

DEST = padovani@pianeta.di.unito.it:public_html/Software/$(NAME)/

all: FuSe.html
	for i in $(DIRS); do make -C $$i; done

dist: $(BASE).tar.gz

$(BASE).tar.gz: $(ARCHIVE)
	rm -rf $(BASE)
	mkdir $(BASE)
	cp --parents $(ARCHIVE) $(BASE)
	tar cvfz $@ $(BASE)
	rm -rf $(BASE)

FuSe.html: README.md
	multimarkdown $< >$@

docs/FuSe.pdf:
	make -C docs FuSe.pdf

sync_home: $(SYNC_HOME_FILES)
	scp $^ $(DEST)

sync: $(SYNC_FILES)
	make -C html
	scp $^ $(DEST)

clean:
	for i in $(DIRS); do make -C $$i clean; done
	rm -rf $(BASE) $(BASE).tar.gz
