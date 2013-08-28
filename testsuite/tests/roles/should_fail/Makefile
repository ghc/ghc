TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: Roles12

Roles12:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Roles12.hs-boot
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c Roles12.hs
