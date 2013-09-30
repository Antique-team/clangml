# Makefile

C_FILES = hello_stubs.c

OCAML_FILES = hello.ml


OCAML_DIR := $(shell ocamlc -where)


C_INCLUDES = -I $(OCAML_DIR)

C_FLAGS = -c99

LD_FLAGS = -lm -ldl -lasmrun -L $(OCAML_DIR)

CC = gcc

OUTPUT_BINARY_NAME = hello

C_OBJECT_FILES = $(C_FILES:.c=.o)

OCAML_OBJECT_FILES = $(OCAML_FILES:.ml=.o)


all: hello_driver

hello_driver: $(C_OBJECT_FILES) $(OCAML_OBJECT_FILES)
	$(CC) -o $(OUTPUT_BINARY_NAME)  $(C_OBJECT_FILES) $(OCAML_OBJECT_FILES) $(LD_FLAGS) 

.c.o:
	$(CC) $(C_FLAGS) $(C_INCLUDES) -c $<  -o $@

%.o: %.ml
	ocamlopt -output-obj $< -o $@

clean:
	rm -rf $(OUTPUT_BINARY_NAME) \
		$(C_OBJECT_FILES) \
		$(OCAML_OBJECT_FILES) \
		$(OCAML_FILES:.ml=.cmi) \
		$(OCAML_FILES:.ml=.cmx)

