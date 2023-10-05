TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T15953:
	'$(TEST_HC)' $(TEST_HC_OPTS) T15953.hs
	cat T15953.dump-ticked && rm T15953.dump-ticked
	cat T15953.dump-simpl-trace && rm T15953.dump-simpl-trace
	cat T15953.dump-tc && rm T15953.dump-tc
	cat T15953.dump-types && rm T15953.dump-types
	cat T15953.dump-core-stats && rm T15953.dump-core-stats

.PHONY: T15953
