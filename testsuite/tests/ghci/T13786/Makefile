TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T13786
T13786 :
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c -fPIC T13786a.c
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c -fPIC T13786b.c
	cat T13786.script | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -v0 T13786a.o T13786b.o T13786.hs
