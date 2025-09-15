TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_DYN = -dynamic -hisuf dyn_hi -osuf dyn_o

.PHONY: dynamicToo001boot
# -dynamic-too should notice that the interface files for B.hs-boot don't match,
#  and issue an error. This is to check the path calculations are correct for boot files.
dynamicToo001boot:
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c A.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -O -c A.hs
	cp B1.hs-boot B.hs-boot
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c B.hs-boot
	cp B2.hs-boot B.hs-boot
	"$(TEST_HC)" $(TEST_HC_OPTS) $(TEST_HC_OPTS_DYN) -O -c B.hs-boot
	# This step fails because the hash of B1 and B2 is different
	"$(TEST_HC)" $(TEST_HC_OPTS)                     -O -c C.hs -dynamic-too || true

