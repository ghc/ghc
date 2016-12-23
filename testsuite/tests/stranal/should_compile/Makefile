TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T13031:
	echo hello
	'$(TEST_HC)' $(TEST_HC_OPTS) -c -fforce-recomp T13031.hs -ddump-simpl | grep 'Arity='
