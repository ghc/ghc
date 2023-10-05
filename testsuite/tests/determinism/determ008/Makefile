TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

determ008:
	$(RM) A.hi A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -O A.hs
	$(CP) A.hi A.normal.hi
	$(RM) A.hi A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -dinitial-unique=16777215 -dunique-increment=-1 A.hs
	diff A.hi A.normal.hi
