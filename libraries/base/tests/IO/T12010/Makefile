TOP=../../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk
TESTDIR=$(TOP)/../libraries/base/tests/IO/T12010

.PHONY: T12010
T12010:
	'$(HSC2HS)' -I $(TOP)/../libraries/base/include/ -I $(TOP)/../includes/ T12010.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -c $(TESTDIR)/cbits/initWinSock.c
	'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -fno-warn-unsupported-calling-conventions $(TESTDIR)/cbits/initWinSock.o $(TESTDIR)/T12010.hs
	./T12010
