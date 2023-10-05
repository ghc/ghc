TOP=../../../testsuite
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T21336b :
	$(TEST_HC) $(TEST_HC_OPTS) -v0 T21336b.hs
	./T21336b >/dev/full

T21336c :
	$(TEST_HC) $(TEST_HC_OPTS) -v0 T21336c.hs
	./T21336c 2>/dev/full >/dev/full
