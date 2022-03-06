TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T20214:
	"$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) T20214.hs 2>&1 </dev/null | grep "SIMD"
