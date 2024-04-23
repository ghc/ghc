TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

OBJSUFFIX = .o

recomp24656:
	$(RM) A.hi A$(OBJSUFFIX)
	'$(TEST_HC)' $(TEST_HC_OPTS) A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -fwrite-if-simplified-core A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface A.hi | grep -A6 "extra decls"

