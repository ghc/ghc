TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: CrossPackage
CrossPackage:
	# TODO do we need the filter-out?
	./prep.bash "$(TEST_HC)" " $(filter-out -rtsopts, $(TEST_HC_OPTS))" "$(GHC_PKG)"
