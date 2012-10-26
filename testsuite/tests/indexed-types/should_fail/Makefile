TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T7354a:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7354b.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c T7354a.hs
