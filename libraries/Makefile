dir = libraries
TOP = ..
SPEC_TARGETS = 1
include $(TOP)/mk/sub-makefile.mk

.PHONY: 1
1 :
	+$(TOPMAKE) stage1_libs

.PHONY: extra-help
help : extra-help
extra-help :
	@echo "  make 1"
	@echo
	@echo "     Build all libraries that are built by the stage1 GHC"
	@echo
