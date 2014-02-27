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

OCAMLBUILD_FLAGS = $(OCAML_BUILD_INCLUDES) -cflags $(OCAML_BUILD_OCAMLOPT_FLAGS) -use-ocamlfind



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

all: hello_lib.o ../ast-processor/main.byte
	$(MAKE) -C plugin

%.o : %.c
	$(CC) $(C_FLAGS) $(C_INCLUDES) -c $<  -o $@

%.o : %.cpp
	$(CPP) $(CPP_FLAGS) $(C_INCLUDES) -c $<  -o $@

%.o : ocaml/_build/%.cmxa
	ocamlfind ocamlopt $< -output-obj -o $@ -linkall -package unix -linkpkg


ocaml/_build/%.cmxa : ocaml/%.mllib ocamlbuild_stage
	cd ocaml && ocamlbuild $(OCAMLBUILD_FLAGS) $(shell basename $@)


../ast-processor/main.byte: $(wildcard ../ast-processor/*.ml* ../ast-processor/*/*.ml* ../ast-processor/*/*/*.ml*)
	$(MAKE) -C $(@D)


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
	./myclang -include "memcad.h" -w test.c

%.test: % all myclang
	./myclang -include "memcad.h" -w $<
	#./myclang -DTEST_ALL -w $<

define testsuite
TESTSUITE.$1 = $2
check-$1: all myclang
	#./myclang -include "memcad.h" -w $$(TESTSUITE.$1)
	./myclang -DTEST_ALL -w $$(TESTSUITE.$1)

check-$1-separate: $$(TESTSUITE.$1:=.test)
endef

$(eval $(call testsuite,testsuite,$(wildcard testsuite/*.[ci])))
$(eval $(call testsuite,memcad,$(wildcard ../ast-processor/memcad/bench/*.c)))

ALDOR_SRC =		\
	abcheck.c	\
	ablogic.c	\
	abnorm.c	\
	abpretty.c	\
	absub.c	\
	absyn.c	\
	abuse.c	\
	archive.c	\
	axlcomp.c	\
	axlobs.c	\
	axlparse.c	\
	bigint.c	\
	bigint_t.c	\
	bitv.c	\
	bitv_t.c	\
	bloop.c	\
	btree.c	\
	btree_t.c	\
	buffer.c	\
	buffer_t.c	\
	ccode.c	\
	ccode_t.c	\
	ccomp.c	\
	cfgfile.c	\
	cmdline.c	\
	compcfg.c	\
	compopt.c	\
	comsg.c	\
	comsgdb.c	\
	cport.c	\
	cport_t.c	\
	debug.c	\
	depdag.c	\
	dflow.c	\
	dnf.c	\
	dnf_t.c	\
	doc.c	\
	dword.c	\
	emit.c	\
	fbox.c	\
	file.c	\
	file_t.c	\
	fint.c	\
	fintphase.c	\
	flatten.c	\
	float_t.c	\
	flog.c	\
	fluid.c	\
	fluid_t.c	\
	fname.c	\
	fname_t.c	\
	foam.c	\
	foamopt.c	\
	foamsig.c	\
	foam_c.c	\
	foam_cfp.c	\
	foam_i.c	\
	forg.c	\
	format.c	\
	format_t.c	\
	fortran.c	\
	freevar.c	\
	ftype.c	\
	genc.c	\
	gencpp.c	\
	genfoam.c	\
	genlisp.c	\
	gf_add.c	\
	gf_excpt.c	\
	gf_fortran.c	\
	gf_gener.c	\
	gf_implicit.c	\
	gf_imps.c	\
	gf_prog.c	\
	gf_reference.c	\
	gf_rtime.c	\
	gf_seq.c	\
	gf_syme.c	\
	include.c	\
	inlutil.c	\
	intset.c	\
	lib.c	\
	linear.c	\
	link_t.c	\
	list.c	\
	list_t.c	\
	loops.c	\
	macex.c	\
	memclim.c	\
	msg.c	\
	msg_t.c	\
	of_argsub.c	\
	of_cfold.c	\
	of_comex.c	\
	of_cprop.c	\
	of_deada.c	\
	of_deadv.c	\
	of_emerg.c	\
	of_env.c	\
	of_hfold.c	\
	of_inlin.c	\
	of_jflow.c	\
	of_killp.c	\
	of_loops.c	\
	of_peep.c	\
	of_retyp.c	\
	of_retyp2.c	\
	of_rrfmt.c	\
	of_util.c	\
	opsys.c	\
	opsys_t.c	\
	optfoam.c	\
	opttools.c	\
	ostream.c	\
	output.c	\
	parseby.c	\
	path.c	\
	phase.c	\
	priq.c	\
	priq_t.c	\
	scan.c	\
	scobind.c	\
	sefo.c	\
	sexpr.c	\
	simpl.c	\
	spesym.c	\
	srcline.c	\
	srcpos.c	\
	stab.c	\
	stdc.c	\
	store.c	\
	store1_t.c	\
	store2_t.c	\
	store3_t.c	\
	strops.c	\
	strops_t.c	\
	symbol.c	\
	symbol_t.c	\
	symcoinfo.c	\
	syme.c	\
	syscmd.c	\
	table.c	\
	table_t.c	\
	tconst.c	\
	termtype.c	\
	terror.c	\
	test.c	\
	textansi.c	\
	textcolour.c	\
	texthp.c	\
	tfcond.c	\
	tform.c	\
	tfsat.c	\
	timer.c	\
	tinfer.c	\
	ti_bup.c	\
	ti_decl.c	\
	ti_sef.c	\
	ti_tdn.c	\
	ti_top.c	\
	token.c	\
	tposs.c	\
	tqual.c	\
	usedef.c	\
	util.c	\
	util_t.c	\
	version.c	\
	xfloat.c	\
	xfloat_t.c
$(eval $(call testsuite,aldor,$(addprefix ../../github/_build/src/lang/aldor/compiler/,$(ALDOR_SRC))))
