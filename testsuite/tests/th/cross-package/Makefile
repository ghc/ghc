TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# TODO it works even without -package obj, but it should complain about the package not being exposed
DB := -package-db db -package dep
BASIC := $(TEST_HC_OPTS) $(DB) -this-unit-id=cross -v0
BC := -fprefer-byte-code -fbyte-code-and-object-code
ARGS := $(BASIC) $(BC)

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

.PHONY: CrossPackageMultiUnit
CrossPackageMultiUnit:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" 1
	mkdir -p unit2-src/
	mv CrossLocal.hs CrossNum.hs CrossNum.hs-boot unit2-src/
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ARGS) -unit @unit1 -unit @unit2
	./Cross
