# Makefile

C_FILES = hello_stubs.c

CPP_FILES = hello_cpp.cpp


# Traverse all ocaml src directories recursively to find additional source directories
# but don't traverse directories starting with '.'

OCAML_TOP_LEVEL_SRC_DIRECTORIES = $(shell cd ocaml && ls -d */ | grep -v _build)

OCAML_SRC_DIRECTORIES_WITH_TRAILING_SLASH =  $(foreach dir,$(OCAML_TOP_LEVEL_SRC_DIRECTORIES), $(shell cd ocaml && find $(dir) -name '\.*' -prune -o -type d -print))

OCAML_SRC_DIRECTORIES = $(patsubst %/,%,$(OCAML_SRC_DIRECTORIES_WITH_TRAILING_SLASH))


OCAML_INSTALL_DIR := $(shell ocamlc -where)


# Comma separated to make ocamlbuild happy
OCAML_BUILD_OCAMLOPT_FLAGS = -warn-error,A,-annot

OCAML_BUILD_INCLUDES = $(addprefix -I ,$(OCAML_SRC_DIRECTORIES))

OCAMLBUILD_FLAGS = $(OCAML_BUILD_INCLUDES) -cflags $(OCAML_BUILD_OCAMLOPT_FLAGS)



C_INCLUDES = -I $(OCAML_INSTALL_DIR)

C_FLAGS = 
CPP_FLAGS = -std=c++11

LD_FLAGS = -v -L $(OCAML_INSTALL_DIR) -lm -ldl -lasmrun -lstdc++ -lcrt1.o -macosx_version_min 10.7

CC = clang
CPP = clang++


OUTPUT_BINARY_NAME = hello

C_OBJECT_FILES = $(C_FILES:.c=.o)
CPP_OBJECT_FILES = $(CPP_FILES:.cpp=.o)

OCAML_OBJECT_PATHS = $(OCAML_SRC_PATHS:.ml=.o)


OCAML_BUILD_DIR = ocaml/_build

OCAML_LIBRARY_MODULE_LIST_FILE_BASE = hello_lib

OCAML_COMBINED_OBJECT_FILE = ocaml_combined.o


CLANG_DOWNLOAD_DIR = ext/clang
CLANG_BUILD_DIR = $(CLANG_DOWNLOAD_DIR)/build


.PHONY: ocamlbuild_stage

all: hello_lib.o
	$(MAKE) -C plugin

%.o : %.c
	$(CC) $(C_FLAGS) $(C_INCLUDES) -c $<  -o $@

%.o : %.cpp
	$(CPP) $(CPP_FLAGS) $(C_INCLUDES) -c $<  -o $@

 %.o : ocaml/_build/%.cmxa
	ocamlopt $< -output-obj -o $@ -linkall


ocaml/_build/%.cmxa : ocaml/%.mllib ocamlbuild_stage
	cd ocaml && ocamlbuild $(OCAMLBUILD_FLAGS) $(shell basename $@)


# Build all the .cmxa files need to make the library specified in $(OCAML_LIBRARY_MODULE_LIST_FILE_BASE).mllib
# A phony target so that we always run ocamlbuild
ocamlbuild_stage:


clang: clang_build

# Use make ENABLE_OPTIMIZED=1 for optimized clang (10x speed improvement)
clang_build: clang_download
	mkdir -p $(CLANG_BUILD_DIR)
	cd $(CLANG_BUILD_DIR) && ../llvm/configure && make -j 4

clang_download:
	mkdir -p $(CLANG_DOWNLOAD_DIR)
	
	cd $(CLANG_DOWNLOAD_DIR) && svn co http://llvm.org/svn/llvm-project/llvm/trunk llvm
	cd $(CLANG_DOWNLOAD_DIR)/llvm/tools && svn co http://llvm.org/svn/llvm-project/cfe/trunk clang
	cd $(CLANG_DOWNLOAD_DIR)/llvm/projects && svn co http://llvm.org/svn/llvm-project/compiler-rt/trunk compiler-rt
	

clean:
	rm -rf $(OUTPUT_BINARY_NAME) \
		$(C_OBJECT_FILES) \
		$(CPP_OBJECT_FILES) \
		$(OCAML_LIBRARY_MODULE_LIST_FILE_BASE).o
	cd ocaml && ocamlbuild -clean 
	$(MAKE) -C plugin clean


check: all myclang test.c
	./myclang test.c
