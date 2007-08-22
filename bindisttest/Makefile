
.PHONY: default_target

default_target: all

# Ideally we'd just include something to give us variables
# for paths and arguments to tools etc, and those set in mk/build.mk.
TOP=..
include $(TOP)/mk/boilerplate.mk

INST_DIR = $(FPTOOLS_TOP_ABS)/bindisttest/installed

all:
	$(RM) -rf installed
	$(RM) -rf ghc*
	$(RM) HelloWorld HelloWorld.o HelloWorld.hi output
	$(TAR) -jxf $(BIN_DIST_TARBALL)
	cd $(BIN_DIST_NAME) && ./configure --prefix=$(INST_DIR)
	cd $(BIN_DIST_NAME) && make install
	$(INST_DIR)/bin/ghc --make HelloWorld
	./HelloWorld > output
	$(CONTEXT_DIFF) output expected_output

clean distclean:
	$(RM) -rf installed
	$(RM) -rf ghc*
	$(RM) HelloWorld HelloWorld.o HelloWorld.hi output

# Ignore a load of other standard targets
install install-docs doc:
	@:

