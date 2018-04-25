TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_DYN = -dynamic -hisuf dyn_hi -osuf dyn_o

.PHONY: dynamicToo001
# -dynamic-too should notice that the interface files for B don't match,
# and so compile the dyn way separately. This means that the right value
# of B.b should get inlined.
dynamicToo001:
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c A.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -O -c A.hs
	cp B1.hs B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c B.hs
	cp B2.hs B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -O -c B.hs
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c C.hs -dynamic-too
	"$(TEST_HC)" $(TEST_HC_OPTS) A.o     B.o     C.o     -o s
	"$(TEST_HC)" $(TEST_HC_OPTS) A.dyn_o B.dyn_o C.dyn_o -o d
	./s
	./d

