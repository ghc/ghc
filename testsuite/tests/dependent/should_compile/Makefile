TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T13938:
	$(RM) T13938a.hi T13938a.o T13938.hi T13938.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T13938a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T13938.hs
