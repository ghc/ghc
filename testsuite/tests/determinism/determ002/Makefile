TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

determ002:
	$(RM) A.hi A.o
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) A.hs
	$(CP) A.hi A.old.hi
	$(RM) A.o
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) A.hs
	diff A.hi A.old.hi
