TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T15261a:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -with-rtsopts="-t -s" --make T15261a.hs
	./T15261a +RTS --info | grep "rtsopts"

T15261b:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T15261b.hs
	./T15261b +RTS --info | grep "rtsopts"
