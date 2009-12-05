TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

layout001 layout002: %:
	$(RM) $*.hi $*.o
	echo Running with -XNoAlternativeLayoutRule
	-"$(TEST_HC)" -XNoAlternativeLayoutRule -c $*.hs 2>&1
	$(RM) $*.hi $*.o
	echo Running with -XAlternativeLayoutRule
	-"$(TEST_HC)" -XAlternativeLayoutRule -c $*.hs 2>&1

