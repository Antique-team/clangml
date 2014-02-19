all: main.byte

main.byte: $(wildcard *.ml*) _tags
	ocamlbuild -use-ocamlfind $@

clean:
	ocamlbuild -clean
