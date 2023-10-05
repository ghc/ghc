TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_NO_RTSOPTS = $(filter-out -rtsopts,$(TEST_HC_OPTS))

clean:
	rm -f *.hi *.hi-fat *.o

T22405: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -fbyte-code-and-object-code Main
	"$(TEST_HC)" $(TEST_HC_OPTS) -fbyte-code-and-object-code Main

T22405b: clean
	"$(TEST_HC)" $(TEST_HC_OPTS) -fbyte-code-and-object-code Main2 -main-is Main2
	"$(TEST_HC)" $(TEST_HC_OPTS) -fbyte-code-and-object-code Main2 -main-is Main2

