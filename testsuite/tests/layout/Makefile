TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

layout001 layout002 layout003 layout004 layout005 layout006 layout007 layout008 layout009: %:
	$(RM) $*.hi $*.o
	echo Running with -XNoAlternativeLayoutRule
	-"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -XNoAlternativeLayoutRule -c $*.hs 2>&1
	$(RM) $*.hi $*.o
	echo Running with -XAlternativeLayoutRule
	-"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -XAlternativeLayoutRule -c $*.hs 2>&1
	$(RM) $*.hi $*.o
	echo Running with -XAlternativeLayoutRule -XAlternativeLayoutRuleTransitional
	-"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -XAlternativeLayoutRule -XAlternativeLayoutRuleTransitional -c $*.hs 2>&1

