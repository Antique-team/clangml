CXXFLAGS =	\
	-I$(HOME)/usr/include	\
	-D__STDC_CONSTANT_MACROS	\
	-D__STDC_LIMIT_MACROS	\
	-fPIC	\
	-fno-rtti

LDFLAGS =	\
	-Wl,-z,defs	\
	-shared		\
	-L$(HOME)/usr/lib	\
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

build/HelloClangPlugin/Build/Products/Debug/libHelloClangPlugin.dylib: ../hello_lib.o HelloChecker.o PluginRegistration.o hello_cpp.o
	g++ $^ -o $@ $(LDFLAGS)

../hello_lib.o: $(shell find ../ocaml -name "*.ml*")
	$(MAKE) -C ../ocaml

%.o: %.cpp
	$(HOME)/usr/bin/clang++ -c $< $(CXXFLAGS)
