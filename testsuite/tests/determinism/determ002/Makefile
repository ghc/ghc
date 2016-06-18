TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

determ002:
	$(RM) A.hi A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) A.hs
	$(CP) A.hi A.old.hi
	$(RM) A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) A.hs
	diff A.hi A.old.hi
