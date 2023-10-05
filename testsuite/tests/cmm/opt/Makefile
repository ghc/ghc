TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T15188:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O -ddump-cmm-cps -dsuppress-all T15188.cmm | grep if ; echo $$?
