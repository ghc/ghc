TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# TODO check error without -package obj (especially for ObjCode, it appears to fail at link time)
BASIC := $(TEST_HC_OPTS) -this-unit-id=cross -package-db db -package dep -package obj -v0
ARGS := $(BASIC) -fprefer-byte-code -fbyte-code-and-object-code

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

.PHONY: CrossPackageArchiveObjCode
CrossPackageArchiveObjCode:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" 1
	./run.bash "$(TEST_HC)" "$(BASIC)"
