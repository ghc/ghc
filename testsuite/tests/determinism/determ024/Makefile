TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

determ024:
	$(RM) A.hi A.o B.hi B.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 B.hs
	'$(TEST_HC)' --show-iface B.hi > B_clean_iface
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 A.hs B.hs -fforce-recomp
	'$(TEST_HC)' --show-iface B.hi > B_dirty_iface
	diff B_clean_iface B_dirty_iface
