TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

TEST_HC_OPTS_NO_RTSOPTS = $(filter-out -rtsopts,$(TEST_HC_OPTS))

# -----------------------------------------------------------------------------
# One-shot compilations, non-hierarchical modules

# Check that modifying flags doesn't affect interface
SelfRecomp01:
	"$(TEST_HC)" $(TEST_HC_OPTS) SelfRecomp01.hs -fno-write-if-self-recomp -v0
	"$(TEST_HC)" --show-iface SelfRecomp01.hi > iface1
	rm SelfRecomp01.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) SelfRecomp01.hs -fno-write-if-self-recomp -Iidir -v0
	"$(TEST_HC)" --show-iface SelfRecomp01.hi > iface2
	diff iface1 iface2

# Check that the result of addDependentFile doesn't end up in interface
SelfRecomp02:
	"$(TEST_HC)" $(TEST_HC_OPTS) SelfRecomp02.hs -fno-write-if-self-recomp -v0
	"$(TEST_HC)" --show-iface SelfRecomp02.hi > iface
	[ -z $(grep iface SelfRecomp02.hs) ]

# Check that modifying source doesn't affect interface
SelfRecomp03:
	"$(TEST_HC)" $(TEST_HC_OPTS) SelfRecomp03.hs -fno-write-if-self-recomp -v0
	"$(TEST_HC)" --show-iface SelfRecomp03.hi > iface1
	rm SelfRecomp03.hi
	echo "" >> SelfRecomp03.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) SelfRecomp03.hs -fno-write-if-self-recomp -v0
	"$(TEST_HC)" --show-iface SelfRecomp03.hi > iface2
	diff iface1 iface2

# Check that if you don't have recompilation info then you always recompile.
SelfRecomp04:
	"$(TEST_HC)" $(TEST_HC_OPTS) SelfRecomp04.hs -fno-write-if-self-recomp -fhide-source-paths
	"$(TEST_HC)" $(TEST_HC_OPTS) SelfRecomp04.hs -fno-write-if-self-recomp -fhide-source-paths
