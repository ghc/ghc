TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T4150

T4150:
	$(RM) T4150A.hi T4150A.o T4150.hi T4150.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -c T4150A.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -c T4150.hs

