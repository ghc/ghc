TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

determ023:
	$(RM) A.hi A.o
	mkdir -p a
	mkdir -p ab
	cp A.hs a
	cp A.hs ab
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -ia A -hidir=a
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -iab A -hidir=ab
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface a/A.hi > hi1
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface ab/A.hi > hi2
	diff hi1 hi2
