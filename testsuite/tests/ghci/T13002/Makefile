TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: T13002
T13002:
	-'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -fobject-code -ddump-rule-firings -no-ignore-dot-ghci \
	-ghci-script T13002.script < /dev/null
