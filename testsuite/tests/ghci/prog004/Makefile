TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that we can load a C object compiled with -g into GHCi.
# (sourceforge bug #1073501).
ghciprog004:
	rm -f ctest.o
	echo "int foo(){}" >ctest.c
	'$(TEST_HC)' $(TEST_HC_OPTS) -optc-g -c ctest.c
	echo ":q" | '$(TEST_HC)' $(TEST_HC_OPTS) -v0 --interactive -ignore-dot-ghci ctest.o
