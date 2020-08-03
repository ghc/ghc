TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

specializable:
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c SpecNoinlineLib.hs \
	  -fno-worker-wrapper
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c User.hs \
	  -ddump-simpl \
	  -fno-worker-wrapper | \
	  grep -v 1236 # check that there was no inlining

