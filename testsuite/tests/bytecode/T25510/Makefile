TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T25510c:
	'$(TEST_HC)' $(EXTRA_HC_OPTS) $(ghcThWayFlags) -fhpc -fbyte-code-and-object-code -c T25510A.hs
	'$(TEST_HC)' $(EXTRA_HC_OPTS) $(ghcThWayFlags) -fhpc -fprefer-byte-code -c T25510B.hs
