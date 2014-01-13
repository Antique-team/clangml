LIBDIR = $(HOME)/.opam/4.01.0+PIC/lib/ocaml

INCLUDE_PART=-I,$(LIBDIR)/camlp4,-I,$(LIBDIR)/camlp4/Camlp4Parsers

LINK_PART=$(LIBDIR)/dynlink.cma,$(LIBDIR)/camlp4/camlp4fulllib.cma

all: main.byte

main.byte: $(wildcard *.ml*)
	ocamlbuild main.byte -cflags $(INCLUDE_PART) -lflags $(LINK_PART)

clean:
	ocamlbuild -clean
