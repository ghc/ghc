TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

determ006:
	$(RM) spec-inline-determ.hi spec-inline-determ.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -O2 spec-inline-determ.hs
	$(CP) spec-inline-determ.hi spec-inline-determ.old.hi
	$(RM) spec-inline-determ.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -dinitial-unique=16777206 -dunique-increment=-1 -O2 spec-inline-determ.hs
	diff spec-inline-determ.hi spec-inline-determ.old.hi
