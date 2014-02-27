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

$(PLUGIN): ../hello_lib.o ../../camlp4_autogen/cutil/ocaml++.o trace.o OCamlVisitor.o HelloChecker.o PluginRegistration.o bridge_ast.o
	$(EXT)/bin/clang++ $^ -o $@ $(LDFLAGS)

../hello_lib.o: $(shell find ../ocaml -name "*.ml*")
	$(MAKE) -C ../ocaml

clean:
	rm -f $(PLUGIN) *.o bridge_ast.cpp bridge_ast.h

%.o: %.cpp clastAst.h $(wildcard *.h ../../camlp4_autogen/cutil/*.h)
	$(EXT)/bin/clang++ -c $< $(CXXFLAGS) -o $@

OCamlVisitor.o: OCamlVisitor.cpp bridge_ast.h $(wildcard *.h ../../camlp4_autogen/cutil/*.h)
	$(EXT)/bin/clang++ -c $< $(CXXFLAGS) -frtti

bridge_ast.o: bridge_ast.cpp bridge_ast.h $(wildcard *.h ../../camlp4_autogen/cutil/*.h)
	$(EXT)/bin/clang++ -c $< $(CXXFLAGS) -frtti

bridge_ast.h: ../../common/clangAst.ml ../../camlp4_autogen/main.byte
	../../camlp4_autogen/main.byte bridge_ast $<

../../camlp4_autogen/main.byte: $(wildcard ../../camlp4_autogen/*.ml*)
	$(MAKE) -C ../../camlp4_autogen
