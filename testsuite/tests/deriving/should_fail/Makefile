TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

drvfail016:
	$(RM) -f drvfail016.hi-boot drvfail016.o-boot
	'$(TEST_HC)' $(TEST_HC_OPTS) -XGeneralizedNewtypeDeriving -c drvfail016.hs-boot; echo $$?

