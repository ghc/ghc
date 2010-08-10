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

# If the user says 'make' or 'make stage=2' here, we behave as if they were
# in the ghc directory instead, so that the executable GHC gets built.
.PHONY: default_to_ghc all_ghc
default_to_ghc : all_ghc

dir = compiler
TOP = ..
SPEC_TARGETS = 1 2 3
include $(TOP)/mk/sub-makefile.mk

all_ghc :
	+$(TOPMAKE) all_ghc $(EXTRA_MAKE_OPTS)

FAST_MAKE_OPTS += compiler_stage1_NO_BUILD_DEPS=YES compiler_stage2_NO_BUILD_DEPS=YES compiler_stage3_NO_BUILD_DEPS=YES

.PHONY: 1 2 3

1:
	+$(TOPMAKE) stage=1 all_ghc_stage1 $(FAST_MAKE_OPTS) NO_STAGE1_DEPS=YES NO_STAGE2_DEPS=YES

2:
	+$(TOPMAKE) stage=2 all_ghc_stage2 $(FAST_MAKE_OPTS) NO_STAGE2_DEPS=YES

3:
	+$(TOPMAKE) stage=3 all_ghc_stage3 $(FAST_MAKE_OPTS) NO_STAGE3_DEPS=YES

.PHONY: extra-help
help : extra-help
extra-help :
	@echo "  make 1"
	@echo "  make 2"
	@echo "  make 3"
	@echo
	@echo "     Build the stage 1, 2 or 3 GHC respectively, omitting dependencies"
	@echo "     and initial phases for speed."
