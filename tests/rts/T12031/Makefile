TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T12031:
	'$(TEST_HC)' -c bar.c -o bar.o
	'$(TEST_HC)' -c baz.c -o baz.o
	echo bar | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) bar.o baz.o ExternBug.hs
