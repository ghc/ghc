TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

checkExists = [ -f $1 ] || echo $1 missing
checkMissing = [ ! -f $1 ] || echo $1 exists

.PHONY: dynamicToo003
# Check that "-c -dynamic-too" works
dynamicToo003:
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 -c A003.hs
	$(call checkExists,A003.o)
	$(call checkExists,A003.hi)
	$(call checkExists,A003.dyn_o)
	$(call checkExists,A003.dyn_hi)

.PHONY: dynamicTooRecomp
# Check that recompilation with "-c -dynamic-too" works
dynamicTooRecomp:
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 -c ARecomp.hs
	# Remove just dynamic objects
	$(RM) ARecomp.dyn*
	# Recompile
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 -c ARecomp.hs
	# Check everything is there
	$(call checkExists,ARecomp.o)
	$(call checkExists,ARecomp.hi)
	$(call checkExists,ARecomp.dyn_o)
	$(call checkExists,ARecomp.dyn_hi)

.PHONY: dynamicTooOnlyInterface
# Check that a missing .dyn_o does not cause recompilation when `-fno-code` `-fwrite-interface`
# is combined
dynamicTooOnlyInterface:
	"$(TEST_HC)" $(TEST_HC_OPTS) -dynamic-too -v0 -c ARecomp.hs
	# Remove just dynamic objects
	$(RM) ARecomp.dyn*
	# Recompile, using -fno-code -fwrite-interface
	"$(TEST_HC)" $(TEST_HC_OPTS) -fno-code -fwrite-interface -v0 -c ARecomp.hs
	# Check the right things are there
	$(call checkExists,ARecomp.o)
	$(call checkExists,ARecomp.hi)
	$(call checkMissing,ARecomp.dyn_o)
	$(call checkMissing,ARecomp.dyn_hi)

