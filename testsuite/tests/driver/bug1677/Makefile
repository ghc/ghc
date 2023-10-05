TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

bug1677 :
	'$(TEST_HC)' $(TEST_HC_OPTS) -M -dep-suffix "" Foo.hs Bar.hs || true

