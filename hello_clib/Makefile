# Makefile

C_FILES = hello_stubs.c

CPP_FILES = hello_cpp.cpp

OCAML_UTIL_SRC_FILES = util/formatx.ml

OCAML_SRC_FILES = hello_ast.ml $(OCAML_UTIL_SRC_FILES) hello_pp.ml hello.ml

OCAML_SRC_PATHS = $(addprefix ocaml/, $(OCAML_SRC_FILES))

OCAML_DIR := $(shell ocamlc -where)

OCAML_FLAGS = -I ocaml -I ocaml/util -warn-error A -annot


C_INCLUDES = -I $(OCAML_DIR)

C_FLAGS = 
CPP_FLAGS = -std=c++11

LD_FLAGS = -v -L $(OCAML_DIR) -lm -ldl -lasmrun -lstdc++ -lcrt1.o -macosx_version_min 10.7

CC = clang
CPP = clang++


OUTPUT_BINARY_NAME = hello






C_OBJECT_FILES = $(C_FILES:.c=.o)
CPP_OBJECT_FILES = $(CPP_FILES:.cpp=.o)

OCAML_OBJECT_PATHS = $(OCAML_SRC_PATHS:.ml=.o)


OCAML_BUILD_DIR = ocaml/_build


all: hello_driver

build:
	mkdir -p build/ocaml


# /usr/local/lib/ocaml/stdlib.a

hello_driver: build $(C_OBJECT_FILES) $(CPP_OBJECT_FILES) ocaml_combined_object.o
	ld -o $(OUTPUT_BINARY_NAME) ocaml_combined_object.o $(C_OBJECT_FILES) $(CPP_OBJECT_FILES)   $(LD_FLAGS) 

%.o : %.c
	$(CC) $(C_FLAGS) $(C_INCLUDES) -c $<  -o $@

%.o : %.cpp
	$(CPP) $(CPP_FLAGS) $(C_INCLUDES) -c $<  -o $@

ocaml_combined_object.o: $(OCAML_SRC_PATHS)
	ocamlopt $(OCAML_FLAGS) $(OCAML_SRC_PATHS) -output-obj -o ocaml_combined_object.o


clean:
	rm -rf $(OUTPUT_BINARY_NAME) \
		$(C_OBJECT_FILES) \
		$(CPP_OBJECT_FILES) \
		$(OCAML_OBJECT_PATHS) \
		$(OCAML_SRC_PATHS:.ml=.cmi) \
		$(OCAML_SRC_PATHS:.ml=.cmx) \
		ocaml_combined_object.o \
		build

