TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: Roles12

Roles12:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Roles12.hs-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Roles12a.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c Roles12.hs

T9204:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T9204.hs-boot
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c T9204a.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c T9204.hs
