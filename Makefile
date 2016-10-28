.PHONY: clean wc install uninstall reinstall

MAIN = mainClang

TARGETS =					\
	clangml.dylib				\
	consumer/processor.native

ALL_FILES := $(shell find */ -type f -not -wholename "_build/*" \
	                        -and -not -wholename "ocaml-4.01.0/*" \
	                        -and -not -wholename "big-tests/*" \
				) myocamlbuild.ml Makefile


$(MAIN).native: $(ALL_FILES)
	ocamlbuild -classic-display -cflags -annot $(TARGETS) -package bytes
	@touch $@

clean:
	ocamlbuild -clean
	\rm -f mainClang.native

wc:
	@find */					\
	  -not -wholename "_build/*"		-and	\
	  -not -wholename "plugin/testsuite/*"	-and	\
	  -type f					\
	  | sort | xargs wc -l


install:
	ocamlfind install clangml META		\
		_build/clangml.dylib		\
		_build/clang/*.cm[iox]		\
		_build/clang/*.o		\
		_build/clang/clang/*.mli	\
		_build/clang/clang/ast.ml	\
		_build/util.cm[iox]		\
		_build/util.o

uninstall:
	ocamlfind remove clangml

reinstall:
	@$(MAKE) uninstall
	@$(MAKE) install


# tags for C++ files
CPP_FILES := $(shell find */ -type f -not -wholename "_build/*" \
			-and \( -wholename "*.cpp" -or -wholename "*.h" \) )

cpp_etags: $(CPP_FILES)
	etags -o cpp_etags $(CPP_FILES)

# tags for OCaml files
ML_FILES := $(shell find */ -type f -not -wholename "_build/*" \
                -and \( -wholename "*.ml" -or -wholename "*.mli" \) \
                -and -not -wholename "tools/simplify/toSimple.ml" \
                -and -not -wholename "tools/ocamlTypes/codegen.ml" \
                -and -not -wholename "tools/ocamlTypes/parse.ml" \
                -and -not -wholename "tools/visitgen/generateVisitor.ml" )


DERIVING_ROOT := $(shell ocamlfind -query deriving)

ml_etags: $(ML_FILES)
	otags -pa $(DERIVING_ROOT)/pa_deriving_common.cma \
              -pa $(DERIVING_ROOT)/pa_deriving_std.cma \
              -pa $(DERIVING_ROOT)/pa_deriving_classes.cma \
              -o ml_etags $(ML_FILES)

doxygen_doc: Doxyfile
	doxygen


####################################################################
## Testing
####################################################################

ALDOR_PATH = ../github/_build/src/lang/aldor

CPPFLAGS =				\
	-D_GNU_SOURCE			\
	-D_ANALYSING			\
	-DTEST_ALL			\
	-DSTO_USE_MALLOC		\
	-I$(ALDOR_PATH)/compiler	\
	-I$(ALDOR_PATH)/compiler/java

CCFLAGS =				\
	$(CPPFLAGS)			\
	-Wall				\
	-Wextra				\
	-Wfatal-errors			\
	-Wno-unused-function		\
	-Wno-unused-parameter		\
	-Wno-sign-compare		\
	-Wno-missing-field-initializers	\
	#

#CCFLAGS +=				\
	-ansi				\
	-pedantic			\
	#

CLANGFLAGS =				\
	$(CCFLAGS)			\
	-fcolor-diagnostics		\
	-Wno-typedef-redefinition	\
	#

GCCFLAGS =				\
	$(CCFLAGS)			\
	-Wsuggest-attribute=pure	\
	-Wsuggest-attribute=const	\
	-Wsuggest-attribute=noreturn	\
	-Wsuggest-attribute=format	\
	-Wno-empty-body			\
	-Wno-unused-but-set-variable	\
	#

#CLANGFLAGS +=				\
	-Wconversion			\
	-Wno-sign-conversion
