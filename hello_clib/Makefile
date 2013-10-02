# Makefile

C_FILES = hello_stubs.c

CPP_FILES = hello_cpp.cpp

OCAML_FILES = hello.ml


OCAML_DIR := $(shell ocamlc -where)


C_INCLUDES = -I $(OCAML_DIR)

C_FLAGS = -c99
CPP_FLAGS = -std=c++11

LD_FLAGS = -L $(OCAML_DIR) -lm -ldl -lasmrun -lstdc++ -lcrt1.o  -macosx_version_min 10.7

CC = clang
CPP = clang++

OUTPUT_BINARY_NAME = hello

C_OBJECT_FILES = $(C_FILES:.c=.o)
CPP_OBJECT_FILES = $(CPP_FILES:.cpp=.o)

OCAML_OBJECT_FILES = $(OCAML_FILES:.ml=.o)


all: hello_driver

hello_driver: $(C_OBJECT_FILES) $(CPP_OBJECT_FILES) $(OCAML_OBJECT_FILES)
	ld -o $(OUTPUT_BINARY_NAME) $(C_OBJECT_FILES) $(CPP_OBJECT_FILES) $(OCAML_OBJECT_FILES) $(LD_FLAGS) 

%.o : %.c
	$(CC) $(C_FLAGS) $(C_INCLUDES) -c $<  -o $@

%.o : %.cpp
	$(CPP) $(CPP_FLAGS) $(C_INCLUDES) -c $<  -o $@

%.o: %.ml
	ocamlopt -output-obj $< -o $@

clean:
	rm -rf $(OUTPUT_BINARY_NAME) \
		$(C_OBJECT_FILES) \
		$(CPP_OBJECT_FILES) \
		$(OCAML_OBJECT_FILES) \
		$(OCAML_FILES:.ml=.cmi) \
		$(OCAML_FILES:.ml=.cmx)

