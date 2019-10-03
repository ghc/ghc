TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Uses Makefile to ensure that the testsuite driver doesn't normalize away the ./
T12674:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -o T12674 ./-T12674.hs ././-T12674c.c
	./T12674

T12674w:
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -o T12674 .\\\-T12674.hs .\\\.\\\-T12674c.c
	./T12674

