TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Check that the instance order returned by TH's reifyInstances doesn't
# depend on the order of Uniques (#27459): run a splice that prints the
# instances with two unique supplies and compare the output.
determ026:
	$(RM) A.hi A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -dinitial-unique=0 -dunique-increment=1 A.hs > A.out.normal
	$(RM) A.hi A.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -dinitial-unique=16777215 -dunique-increment=-1 A.hs > A.out.reversed
	diff A.out.normal A.out.reversed
