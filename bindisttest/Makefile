
.PHONY: default_target

default_target: all

# Ideally we'd just include something to give us variables
# for paths and arguments to tools etc, and those set in mk/build.mk.
TOP=..
include $(TOP)/mk/boilerplate.mk

all:
	$(RM) -rf $(BIN_DIST_INST_SUBDIR)
	$(RM) -rf ghc*
	$(RM) HelloWorld HelloWorld.o HelloWorld.hi output
	# --force-local makes tar not think that c:/foo refers to a remote file
	$(TAR) --force-local -jxf $(FPTOOLS_TOP_ABS)/$(BIN_DIST_TAR_BZ2)
ifeq "$(Windows)" "YES"
	mv $(BIN_DIST_NAME) $(BIN_DIST_INST_DIR)
else
	cd $(BIN_DIST_NAME) && ./configure --prefix=$(BIN_DIST_INST_DIR)
	cd $(BIN_DIST_NAME) && make install
endif
	$(BIN_DIST_INST_DIR)/bin/ghc --make HelloWorld
	./HelloWorld > output
	$(CONTEXT_DIFF) output expected_output

clean distclean:
	$(RM) -rf $(BIN_DIST_INST_SUBDIR)
	$(RM) -rf ghc*
	$(RM) HelloWorld HelloWorld.o HelloWorld.hi output

# Ignore a load of other standard targets
install install-docs doc:
	@:

