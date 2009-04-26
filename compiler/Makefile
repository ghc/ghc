dir = compiler
TOP = ..
SPEC_TARGETS = 1 2 3
include $(TOP)/mk/sub-makefile.mk

.PHONY: 1 2 3

1:
	+$(TOPMAKE) stage=1 all_ghc_stage1 compiler_stage1_NO_BUILD_DEPS=YES

2:
	+$(TOPMAKE) stage=2 all_ghc_stage2 compiler_stage2_NO_BUILD_DEPS=YES

3:
	+$(TOPMAKE) stage=3 all_ghc_stage3 compiler_stage3_NO_BUILD_DEPS=YES

