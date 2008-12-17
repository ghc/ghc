TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# The libraries that we actually know about. We don't want to test
# extralibs that are in our tree but which we haven't built.
ifeq "$(NEWBUILD)" "YES"
LIBRARIES := $(shell $(FPTOOLS_TOP_ABS)/$(GHC_PKG_INPLACE) list --simple-output --names-only)
else
LIBRARIES := $(shell $(GHC_PKG) list --simple-output --names-only)
endif

# Now find the "tests" directories of those libraries, where they exist
LIBRARY_TEST_PATHS := $(wildcard $(patsubst %, $(FPTOOLS_TOP)/libraries/%/tests, $(LIBRARIES)))

# Add tests from packages
RUNTEST_OPTS += $(patsubst %, --rootdir=%, $(LIBRARY_TEST_PATHS))
