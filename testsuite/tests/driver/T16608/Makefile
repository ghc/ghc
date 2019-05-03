TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T16608:
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -O0 T16608.hs
	sed -i -e 's/{- . succ -}/. succ/' MyInteger.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --make -O0 T16608.hs
