TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

boot1:
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -c A.hs-boot B.hs

boot2:
	'$(TEST_HC)' $(EXTRA_HC_OPTS) A.hs-boot A.hs B.hs -v0

boot3:
	'$(TEST_HC)' $(EXTRA_HC_OPTS) A.hs-boot B.hs -v0
