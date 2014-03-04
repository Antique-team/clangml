all:

%:
	$(MAKE) -C ast-processor $@
	$(MAKE) -C camlp4_autogen $@
	$(MAKE) -C clang_plugin $@
