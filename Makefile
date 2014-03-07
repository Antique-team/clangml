OCAMLBUILD_FLAGS =				\
	-cflags -I,+camlp4/Camlp4Parsers	\
	-use-ocamlfind

TARGETS =		\
	clangaml.dylib	\
	processor.native

build:
	ocamlbuild $(OCAMLBUILD_FLAGS) $(TARGETS)

clean:
	ocamlbuild -clean
