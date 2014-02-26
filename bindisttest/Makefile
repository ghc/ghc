# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

.PHONY: default_target

default_target: all

# Ideally we'd just include something to give us variables
# for paths and arguments to tools etc, and those set in mk/build.mk.
TOP=..
include $(TOP)/mk/tree.mk
include $(TOP)/mk/config.mk

ifeq "$(TEST_PREP)" "YES"
BIN_DIST_TEST_TAR_COMP = ../$(BIN_DIST_PREP_TAR_COMP)
else
BIN_DIST_TEST_TAR_COMP = ../$(BIN_DIST_TAR_COMP)
endif

all:
	"$(RM)" $(RM_OPTS_REC) $(BIN_DIST_INST_SUBDIR)
	"$(RM)" $(RM_OPTS_REC) a/b/c/*
	"$(RM)" $(RM_OPTS) HelloWorld HelloWorld.o HelloWorld.hi output
# We use the a/b/c subdirectory as configure looks for install-sh in
# . .. ../.. and we don't want it to find the build system's install-sh.
#
# NB. tar has funny interpretation of filenames sometimes (thinking
# c:/foo is a remote file), so it's safer to bzip and then pipe into
# tar rather than using tar -xjf:
	cd a/b/c/ && $(TAR_COMP_CMD) -cd ../../../$(BIN_DIST_TEST_TAR_COMP) | $(TAR_CMD) -xf -
ifeq "$(Windows)" "YES"
	mv a/b/c/$(BIN_DIST_NAME) $(BIN_DIST_INST_DIR)
else
	cd a/b/c/$(BIN_DIST_NAME) && ./configure --prefix=$(BIN_DIST_INST_DIR)
	cd a/b/c/$(BIN_DIST_NAME) && $(MAKE) install
endif
	$(BIN_DIST_INST_DIR)/bin/runghc HelloWorld > output
	$(CONTEXT_DIFF) output expected_output
	$(BIN_DIST_INST_DIR)/bin/ghc --make HelloWorld
	./HelloWorld > output
	$(CONTEXT_DIFF) output expected_output
# Without --no-user-package-db we might pick up random packages from ~/.ghc
	$(BIN_DIST_INST_DIR)/bin/ghc-pkg check --no-user-package-db

clean distclean:
	"$(RM)" $(RM_OPTS_REC) $(BIN_DIST_INST_SUBDIR)
	"$(RM)" $(RM_OPTS_REC) a/b/c/*
	"$(RM)" $(RM_OPTS) HelloWorld HelloWorld.o HelloWorld.hi output

# Ignore a load of other standard targets
install install-docs doc:
	@:

show:
	@echo '$(VALUE)="$($(VALUE))"'

