TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T16197
T16197:
	'$(TEST_HC)' -O0 -v0 T16197.hs
	./T16197
	rm T16197.o T16197.hi T16197
	'$(TEST_HC)' -O1 -v0 T16197.hs
	./T16197
