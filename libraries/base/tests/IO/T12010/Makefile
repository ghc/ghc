TOP=../../../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T12010
T12010:
	'$(HSC2HS)' -I ../../../include/ -I ../../../../../includes/ T12010.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -c cbits/initWinSock.c
	'$(TEST_HC)' $(TEST_HC_OPTS) $(WAY_FLAGS) -fno-warn-unsupported-calling-conventions cbits/initWinSock.o T12010.hs
	./T12010
