TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T15012:
	$(RM) T15012.hi T15012.o T15012a.hi T15012a.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T15012a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T15012.hs
