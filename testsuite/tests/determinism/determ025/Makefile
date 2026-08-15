TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Check that error messages don't depend on the order of Uniques (#27459):
# compile a module producing an ambiguity error with two unique supplies
# and compare the errors.
determ025:
	$(RM) A.hi A.o
	-'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -dinitial-unique=0 -dunique-increment=1 A.hs 2> A.err.normal
	$(RM) A.hi A.o
	-'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -dinitial-unique=16777215 -dunique-increment=-1 A.hs 2> A.err.reversed
	diff A.err.normal A.err.reversed
	$(RM) A.hi A.o
	-'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -fprint-potential-instances -dinitial-unique=0 -dunique-increment=1 A.hs 2> A.err.all.normal
	$(RM) A.hi A.o
	-'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -fprint-potential-instances -dinitial-unique=16777215 -dunique-increment=-1 A.hs 2> A.err.all.reversed
	diff A.err.all.normal A.err.all.reversed
