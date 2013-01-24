TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

drvfail016:
	$(RM) -f drvfail016.hi-boot drvfail016.o-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -XGeneralizedNewtypeDeriving -c drvfail016.hs-boot; echo $$?

.PHONY: T1133A
T1133A:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T1133A.hs-boot
	-'$(TEST_HC)' $(TEST_HC_OPTS) -c T1133A.hs

