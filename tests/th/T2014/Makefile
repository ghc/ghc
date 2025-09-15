TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T2014 :
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -fforce-recomp -c A.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -fforce-recomp -c A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -fforce-recomp -c B.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -fforce-recomp -c C.hs -v0
