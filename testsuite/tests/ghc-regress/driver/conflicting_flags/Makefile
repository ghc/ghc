TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# If you give -V and --print-libdir together you should get an error.
# The error includes the program name, so we have to sed off the
# version number.

conflicting_flags:
	@$(TEST_HC) -V --print-libdir 2>&1 | sed "s/^ghc-[0-9]*\(\.[0-9]*\)*:/ghc:/"

