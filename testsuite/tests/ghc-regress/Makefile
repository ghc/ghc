TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# The libraries that we actually know about. We don't want to test
# extralibs that are in our tree but which we haven't built.
LIBRARIES := $(shell $(GHC_PKG_PROG) list --simple-output --names-only)
# Now find the "tests" directories of those libraries, where they exist
LIBRARY_TEST_PATHS := $(wildcard $(patsubst %, $(FPTOOLS_TOP)/libraries/%/tests, $(LIBRARIES)))

# Add tests from packages
RUNTEST_OPTS += $(patsubst %, --rootdir=%, $(LIBRARY_TEST_PATHS))
