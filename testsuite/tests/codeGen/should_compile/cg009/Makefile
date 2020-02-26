TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Make sure the LFInfo for an exported, but not directly used newtype
# constructors does not trip up the compiler.
cg009:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O A.hs -fforce-recomp
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -O0 Main.hs -fforce-recomp
