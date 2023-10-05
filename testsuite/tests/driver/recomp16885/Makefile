TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

recomp16885:
	cp M2A.hs M2.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make M.hs
	sleep 1
	cp M2B.hs M2.hs
	-'$(TEST_HC)' $(TEST_HC_OPTS) --make M.hs
