all: main.byte

main.byte: $(wildcard *.ml* */*.ml* */*/*.ml*) _tags
	ocamlbuild -use-ocamlfind $@

clean:
	ocamlbuild -clean
