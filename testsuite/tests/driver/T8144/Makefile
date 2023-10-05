TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that iface hashes are reproducible, and in particular don't include
# timestamps of dependent files. This test case was originally written by Niklas
# Hambüchen; see
#   https://github.com/nh2/ghc-bug-time-dependent-interface-hashes/
#     
clean ::
	rm -f *.o *.hi *.show-iface

dotest ::
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Test.hs -optP-include -optPcabal_macros.h
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface Test.hi > before.show-iface
	sleep 1 # because some systems have a 1-second resolution for file timestamps
	touch cabal_macros.h
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Test.hs -optP-include -optPcabal_macros.h
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface Test.hi > after.show-iface
	diff before.show-iface after.show-iface
