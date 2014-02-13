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
	-L$(HOME)/.opam/4.01.0+PIC/lib/ocaml	\
	-Wl,-rpath,$(HOME)/.opam/4.01.0+PIC/lib/ocaml	\
	-ggdb3

PLUGIN = build/HelloClangPlugin/Build/Products/Debug/libHelloClangPlugin.dylib

$(PLUGIN): ../hello_lib.o ../../camlp4_autogen/cutil/ocaml++.o trace.o OCamlVisitor.o HelloChecker.o PluginRegistration.o hello_cpp.o
	$(EXT)/bin/clang++ $^ -o $@ $(LDFLAGS)

../hello_lib.o: $(shell find ../ocaml -name "*.ml*")
	$(MAKE) -C ../ocaml

clean:
	rm -f $(PLUGIN) *.o hello_cpp.cpp hello_cpp.h

%.o: %.cpp hello_cpp.h $(wildcard *.h ../../camlp4_autogen/cutil/*.h)
	$(EXT)/bin/clang++ -c $< $(CXXFLAGS) -o $@

OCamlVisitor.o: OCamlVisitor.cpp hello_cpp.h $(wildcard *.h ../../camlp4_autogen/cutil/*.h)
	$(EXT)/bin/clang++ -c $< $(CXXFLAGS) -frtti

hello_cpp.o: hello_cpp.cpp hello_cpp.h $(wildcard *.h ../../camlp4_autogen/cutil/*.h)
	$(EXT)/bin/clang++ -c $< $(CXXFLAGS) -frtti

hello_cpp.h: ../ocaml/hello_ast.ml ../../camlp4_autogen/main.byte
	../../camlp4_autogen/main.byte hello_cpp $<

../../camlp4_autogen/main.byte: $(wildcard ../../camlp4_autogen/*.ml*)
	$(MAKE) -C ../../camlp4_autogen
