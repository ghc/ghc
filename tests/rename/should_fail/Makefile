TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T9032:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -fforce-recomp T9032.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c -fforce-recomp -DERR T9032.hs

