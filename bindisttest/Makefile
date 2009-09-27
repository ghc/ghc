# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

.PHONY: default_target

default_target: all

# Ideally we'd just include something to give us variables
# for paths and arguments to tools etc, and those set in mk/build.mk.
TOP=..
include $(TOP)/mk/config.mk

ifeq "$(TEST_PREP)" "YES"
BIN_DIST_TEST_TAR_BZ2 = ../$(BIN_DIST_PREP_TAR_BZ2)
else
BIN_DIST_TEST_TAR_BZ2 = ../$(BIN_DIST_TAR_BZ2)
endif

all:
	$(RM) -rf $(BIN_DIST_INST_SUBDIR)
	$(RM) -rf a/b/c/*
	$(RM) HelloWorld HelloWorld.o HelloWorld.hi output
# We use the a/b/c subdirectory as configure looks for install-sh in
# . .. ../.. and we don't want it to find the build system's install-sh.
# --force-local makes tar not think that c:/foo refers to a remote file
	cd a/b/c/ && $(TAR) --force-local -jxf ../../../$(BIN_DIST_TEST_TAR_BZ2)
ifeq "$(Windows)" "YES"
	mv a/b/c/$(BIN_DIST_NAME) $(BIN_DIST_INST_DIR)
else
	cd a/b/c/$(BIN_DIST_NAME) && ./configure --prefix=$(BIN_DIST_INST_DIR)
	cd a/b/c/$(BIN_DIST_NAME) && make install
endif
	$(BIN_DIST_INST_DIR)/bin/runghc HelloWorld > output
	$(CONTEXT_DIFF) output expected_output
	$(BIN_DIST_INST_DIR)/bin/ghc --make HelloWorld
	./HelloWorld > output
	$(CONTEXT_DIFF) output expected_output
	$(BIN_DIST_INST_DIR)/bin/ghc-pkg check

clean distclean:
	$(RM) -rf $(BIN_DIST_INST_SUBDIR)
	$(RM) -rf a/b/c/*
	$(RM) HelloWorld HelloWorld.o HelloWorld.hi output

# Ignore a load of other standard targets
install install-docs doc:
	@:

