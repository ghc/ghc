TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

## check that there are two "$def" aliases:
#  - one that bitcasts to %T15155_a_closure_struct*
#  - and the other which bitcasts from %T15155_a_closure_struct*
##
cg009:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O A.hs -fforce-recomp
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O0 Main.hs -fforce-recomp
