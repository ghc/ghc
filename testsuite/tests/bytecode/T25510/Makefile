TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T25510c:
	'$(TEST_HC)' $(ghcThWayFlags) -fhpc -fbyte-code-and-object-code -c T25510A.hs
	'$(TEST_HC)' $(ghcThWayFlags) -fhpc -fprefer-byte-code -c T25510B.hs
