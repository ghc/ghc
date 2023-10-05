TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T22042: T22042_clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -O T22042a.hs -dynamic -c
	"$(TEST_HC)" $(TEST_HC_OPTS) -e ":main" T22042.hs T22042a.o

T22042_clean:
	rm -f T22042a.o T22042a.hi

.PHONY: T22042 T22042_clean
