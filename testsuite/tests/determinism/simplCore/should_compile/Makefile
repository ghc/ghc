TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

determ006:
	$(RM) spec-inline-determ.hi spec-inline-determ.o
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -O2 spec-inline-determ.hs
	$(CP) spec-inline-determ.hi spec-inline-determ.old.hi
	$(RM) spec-inline-determ.o
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP) -dinitial-unique=16777206 -dunique-increment=-1 -O2 spec-inline-determ.hs
	diff spec-inline-determ.hi spec-inline-determ.old.hi
