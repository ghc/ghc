TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T17586:
	"$(TEST_HC)" $(TEST_HC_OPTS) -o T17586 ./T17586.hs
	./T17586
	"$(TEST_HC)" $(TEST_HC_OPTS) -o T17586 ./T17586.hs -g3
	./T17586
