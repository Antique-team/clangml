all:

%:
	$(MAKE) -C camlp4_autogen $@
	$(MAKE) -C hello_clang_plugin $@
