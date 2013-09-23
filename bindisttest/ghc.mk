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

ifeq "$(TEST_PREP)" "YES"
BIN_DIST_TEST_TAR_BZ2 = $(BIN_DIST_PREP_TAR_BZ2)
else
BIN_DIST_TEST_TAR_BZ2 = $(BIN_DIST_TAR_BZ2)
endif

.PHONY: test_bindist
test_bindist:
	"$(RM)" $(RM_OPTS_REC) bindisttest/$(BIN_DIST_INST_SUBDIR)
	"$(RM)" $(RM_OPTS_REC) bindisttest/a
	"$(RM)" $(RM_OPTS) bindisttest/HelloWorld
	"$(RM)" $(RM_OPTS) bindisttest/HelloWorld.o
	"$(RM)" $(RM_OPTS) bindisttest/HelloWorld.hi
	"$(RM)" $(RM_OPTS) bindisttest/output
# We use the a/b/c subdirectory as configure looks for install-sh in
# . .. ../.. and we don't want it to find the build system's install-sh.
#
# NB. tar has funny interpretation of filenames sometimes (thinking
# c:/foo is a remote file), so it's safer to bzip and then pipe into
# tar rather than using tar -xjf:
	mkdir bindisttest/a
	mkdir bindisttest/a/b
	mkdir bindisttest/a/b/c
	cd bindisttest/a/b/c/ && $(BZIP2_CMD) -cd ../../../../$(BIN_DIST_TEST_TAR_BZ2) | $(TAR_CMD) -xf -
	$(SHELL) bindisttest/checkBinaries.sh $(ProjectVersion)
ifeq "$(Windows_Host)" "YES"
	mv bindisttest/a/b/c/$(BIN_DIST_NAME) $(BIN_DIST_INST_DIR)
else
	cd bindisttest/a/b/c/$(BIN_DIST_NAME) && ./configure --prefix=$(TOP)/$(BIN_DIST_INST_DIR) --with-gcc="$(WhatGccIsCalled)"
	cd bindisttest/a/b/c/$(BIN_DIST_NAME) && $(MAKE) install
endif
ifeq "$(GhcProfiled)" "NO"
	$(BIN_DIST_INST_DIR)/bin/runghc bindisttest/HelloWorld > bindisttest/output
	$(CONTEXT_DIFF) bindisttest/output bindisttest/expected_output
endif
	$(BIN_DIST_INST_DIR)/bin/ghc --make bindisttest/HelloWorld
	bindisttest/HelloWorld > bindisttest/output
	$(CONTEXT_DIFF) bindisttest/output bindisttest/expected_output
# Without --no-user-package-db we might pick up random packages from ~/.ghc
	$(BIN_DIST_INST_DIR)/bin/ghc-pkg check --no-user-package-db

$(eval $(call clean-target,bindisttest,all,$(BIN_DIST_INST_DIR) $(wildcard bindisttest/a/b/c/*) bindisttest/HelloWorld bindisttest/HelloWorld.o bindisttest/HelloWorld.hi bindisttest/output))

