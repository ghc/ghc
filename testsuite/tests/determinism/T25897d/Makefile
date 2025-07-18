TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T25897d:
	$(RM) A.hi A.o B.hi B.o
	# Use -haddock to get docs: output in the interface file
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -haddock B.hs
	'$(TEST_HC)' --show-iface A.hi > A_clean_iface
	'$(TEST_HC)' --show-iface B.hi > B_clean_iface
	'$(TEST_HC)' $(TEST_HC_OPTS) -dinitial-unique=16777215 -dunique-increment=-1 -v0 -haddock A.hs B.hs -fforce-recomp
	'$(TEST_HC)' --show-iface A.hi > A_dirty_iface
	'$(TEST_HC)' --show-iface B.hi > B_dirty_iface
	diff A_clean_iface A_dirty_iface
	diff B_clean_iface B_dirty_iface
	cat B_clean_iface | grep -A7 "documentation structure"
	cat B_dirty_iface | grep -A7 "documentation structure"

T25897e:
	$(RM) A.hi A.o C.hi C.o
	# Use -haddock to get docs: output in the interface file
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -haddock C.hs
	'$(TEST_HC)' --show-iface A.hi > A_clean_iface
	'$(TEST_HC)' --show-iface C.hi > C_clean_iface
	'$(TEST_HC)' $(TEST_HC_OPTS) -dinitial-unique=16777215 -dunique-increment=-1 -v0 -haddock A.hs C.hs -fforce-recomp
	'$(TEST_HC)' --show-iface A.hi > A_dirty_iface
	'$(TEST_HC)' --show-iface C.hi > C_dirty_iface
	diff A_clean_iface A_dirty_iface
	diff C_clean_iface C_dirty_iface
	cat C_clean_iface | grep -A7 "documentation structure"
	cat C_dirty_iface | grep -A7 "documentation structure"
