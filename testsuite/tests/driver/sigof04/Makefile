TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# -fforce-recomp makes lots of driver tests trivially pass, so we
# filter it out from $(TEST_HC_OPTS).
TEST_HC_OPTS_NO_RECOMP = $(filter-out -fforce-recomp,$(TEST_HC_OPTS))

clean:
	rm -rf containers

sigof04:
	'$(GHC_PKG)' field containers key | sed 's/^.*: *//' > containers
	! '$(TEST_HC)' $(TEST_HC_OPTS_NO_RECOMP)  -c Sig.hs-boot -sig-of "Sig is `cat containers`:Data.Map.Strict"
