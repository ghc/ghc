TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

conflicting_flags:
	'$(TEST_HC)' $(TEST_HC_OPTS) -V --print-libdir 2>&1 | sed "s/, version.*//"

