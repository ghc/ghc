TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

Orphans:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Orphans.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface Orphans.hi | grep -E '^(instance |family instance |"myrule)' | grep -v 'family instance modules:'
