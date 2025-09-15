TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: clean T8184

clean:
	$(RM) *.o *.hi *.hi-boot *.o-boot

T8184: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 --make -O2 C.hs
