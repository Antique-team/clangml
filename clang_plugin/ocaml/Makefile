_build/clangLib.cmxa: $(wildcard *.ml* */*.ml* */*/*.ml*) _tags
	ocamlbuild -cflags -warn-error,A -use-ocamlfind $(@F)
	touch $@
