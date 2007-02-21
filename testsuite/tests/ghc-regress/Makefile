TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Add tests from packages
RUNTEST_OPTS += $(patsubst %, --rootdir=%, $(wildcard $(FPTOOLS_TOP)/libraries/*/tests))
