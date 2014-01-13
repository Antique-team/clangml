all: main.byte

main.byte: $(wildcard *.ml*) _tags
	ocamlbuild -use-ocamlfind $@ -cflags -I,+camlp4/Camlp4Parsers

clean:
	ocamlbuild -clean
