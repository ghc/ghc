TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

determ011:
	$(RM) A.hi A.o
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -dinitial-unique=0 -dunique-increment=1 -O A.hs
	$(CP) A.hi A.normal.hi
	$(RM) A.hi A.o
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -dinitial-unique=16777215 -dunique-increment=-1 -O A.hs
	diff A.hi A.normal.hi
