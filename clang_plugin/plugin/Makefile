EXT = ../ext/clang/build/Debug+Asserts

CXXFLAGS =	\
	-I$(EXT)/include	\
	-I../../camlp4_autogen/cutil	\
	-D__STDC_CONSTANT_MACROS	\
	-D__STDC_LIMIT_MACROS	\
	-fPIC	\
	-fno-rtti	\
	-std=c++11	\
	-ggdb3		\
	-O3

LDFLAGS =	\
	-Wl,-z,defs	\
	-shared		\
	-L$(EXT)/lib	\
	-lclangStaticAnalyzerCore	\
	-lclangAnalysis	\
	-lclangAST	\
	-lclangLex	\
	-lclangBasic	\
	-lLLVMSupport	\
	-lpthread	\
	-ldl	\
	-ltinfo	\
	-lasmrun	\
	-lunix	\
	-L$(HOME)/.opam/4.01.0+PIC/lib/ocaml	\
	-Wl,-rpath,$(HOME)/.opam/4.01.0+PIC/lib/ocaml	\
	-ggdb3

PLUGIN = build/HelloClangPlugin/Build/Products/Debug/libHelloClangPlugin.dylib

OBJECTS =					\
	../ocaml/_build/clangLib.o		\
	../../camlp4_autogen/cutil/ocaml++.o	\
	clang_context.o				\
	clang_operations.o			\
	clang_ref_holder.o			\
	heterogenous_container.o		\
	HelloChecker.o				\
	OCamlVisitor.o				\
	PluginRegistration.o			\
	bridge_ast.o				\
	bridge_cache.o				\
	trace.o

$(PLUGIN): $(OBJECTS)
	$(EXT)/bin/clang++ $^ -o $@ $(LDFLAGS)

../ocaml/_build/clangLib.o: $(wildcard ../ocaml/*.ml* ../ocaml/*/*.ml* ../ocaml/*/*/*.ml*)
	$(MAKE) -C ../ocaml

.PHONY: clean
clean:
	rm -f $(PLUGIN) *.o bridge_ast.cpp bridge_ast.h

%.o: %.cpp bridge_ast.h $(wildcard *.h ../../camlp4_autogen/cutil/*.h)
	$(EXT)/bin/clang++ -c $< $(CXXFLAGS) -o $@

OCamlVisitor.o: OCamlVisitor.cpp bridge_ast.h $(wildcard *.h ../../camlp4_autogen/cutil/*.h)
	$(EXT)/bin/clang++ -c $< $(CXXFLAGS) -frtti

bridge_ast.o: bridge_ast.cpp bridge_ast.h $(wildcard *.h ../../camlp4_autogen/cutil/*.h)
	$(EXT)/bin/clang++ -c $< $(CXXFLAGS) -frtti

bridge_ast.h: ../../common/clangBridge.ml ../../camlp4_autogen/main.byte
	../../camlp4_autogen/main.byte bridge_ast $<

../../common/clangBridge.ml: ../../common/clangAst.ml
	$(MAKE) -C $(@D)

../../camlp4_autogen/main.byte: $(wildcard ../../camlp4_autogen/*.ml*)
	$(MAKE) -C ../../camlp4_autogen
