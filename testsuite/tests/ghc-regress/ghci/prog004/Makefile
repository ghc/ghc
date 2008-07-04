TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test that we can load an object compiled with $(CC) -g into GHCi.
# (sourceforge bug #1073501).
ghciprog004:
	@rm -f ctest.o
	echo "int foo(){}" >ctest.c
	$(CC) -g -c ctest.c
	echo ":q" | $(TEST_HC) -v0 --interactive -ignore-dot-ghci ctest.o
