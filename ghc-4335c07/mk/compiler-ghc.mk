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

TOP = ..
SPEC_TARGETS = 1 2 3 re1 re2 re3
include $(TOP)/mk/sub-makefile.mk

FAST_MAKE_OPTS += compiler_stage1_NO_BUILD_DEPS=YES \
                  compiler_stage2_NO_BUILD_DEPS=YES \
                  compiler_stage3_NO_BUILD_DEPS=YES \
                  ghc_stage1_NO_BUILD_DEPS=YES \
                  ghc_stage2_NO_BUILD_DEPS=YES \
                  ghc_stage3_NO_BUILD_DEPS=YES

.PHONY: 1 2 3

1:
	+$(TOPMAKE) stage=1 all_ghc_stage1 $(FAST_MAKE_OPTS) ONLY_DEPS_FOR="compiler_stage1 ghc_stage1"

2:
	+$(TOPMAKE) stage=2 all_ghc_stage2 $(FAST_MAKE_OPTS) ONLY_DEPS_FOR="compiler_stage2 ghc_stage2" NO_STAGE2_DEPS=YES

3:
	+$(TOPMAKE) stage=3 all_ghc_stage3 $(FAST_MAKE_OPTS) ONLY_DEPS_FOR="compiler_stage3 ghc_stage3" NO_STAGE3_DEPS=YES


# 'make re2' rebuilds stage2, removing the old executable first.  Useful for
# something like 'make re2 GhcDebugged=YES'.

.PHONY: re1 re2 re3
re1:
	$(RM) $(TOP)/ghc/stage1/build/tmp/ghc-stage1
	$(MAKE) 1
re2:
	$(RM) $(TOP)/ghc/stage2/build/tmp/ghc-stage2
	$(MAKE) 2
re3:
	$(RM) $(TOP)/ghc/stage3/build/tmp/ghc-stage3
	$(MAKE) 3

.PHONY: extra-help
help : extra-help
extra-help :
	@echo "  make 1"
	@echo "  make 2"
	@echo "  make 3"
	@echo
	@echo "     Build the stage 1, 2 or 3 GHC respectively, omitting dependencies"
	@echo "     and initial phases for speed."
