TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

ARGS := $(TEST_HC_OPTS) -package-db db -fprefer-byte-code -fbyte-code-and-object-code -package dep -v0

.PHONY: CrossPackageArchive
CrossPackageArchive:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" 1
	./run.bash "$(TEST_HC)" "$(ARGS)"

.PHONY: CrossPackageEmptyArchive
CrossPackageEmptyArchive:
	./prep.bash "$(TEST_HC)" " $(TEST_HC_OPTS)" "$(GHC_PKG)" 2
	./run.bash "$(TEST_HC)" "$(ARGS)"

.PHONY: CrossPackageNoArchive
CrossPackageNoArchive:
	./prep.bash "$(TEST_HC)" " $(TEST_HC_OPTS)" "$(GHC_PKG)" 3
	./run.bash "$(TEST_HC)" "$(ARGS)"
