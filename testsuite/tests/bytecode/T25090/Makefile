TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Verify that the object files aren't linked by clobbering them.
T25090a:
	$(TEST_HC) -c -fbyte-code-and-object-code C.hs-boot
	$(TEST_HC) -c -fbyte-code-and-object-code B.hs
	$(TEST_HC) -c -fbyte-code-and-object-code C.hs
	echo 'corrupt' > B.o
	echo 'corrupt' > C.o
	echo 'corrupt' > C.o-boot
	$(TEST_HC) -c -fbyte-code-and-object-code D.hs
	echo 'corrupt' > D.o
	$(TEST_HC) -c -fbyte-code-and-object-code -fprefer-byte-code A.hs
	$(TEST_HC) -fbyte-code-and-object-code -fprefer-byte-code A.o -o exe
	./exe

T25090b:
	$(TEST_HC) -fbyte-code-and-object-code -fprefer-byte-code A -o exe -v0
	./exe

# TODO it works even without -package obj, but it should complain about the package not being exposed
DB := -package-db db -package dep
BASIC := $(TEST_HC_OPTS) $(DB) -this-unit-id=pkgdep -v0
BC := -fprefer-byte-code -fbyte-code-and-object-code
ARGS := $(BASIC) $(BC)

.PHONY: PkgArchive
PkgArchive:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" 1
	./run.bash "$(TEST_HC)" "$(ARGS)"

.PHONY: PkgEmptyArchive
PkgEmptyArchive:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" 2
	./run.bash "$(TEST_HC)" "$(ARGS)"

.PHONY: PkgNoArchive
PkgNoArchive:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" 3
	./run.bash "$(TEST_HC)" "$(ARGS)"

.PHONY: PkgArchiveObjCode
PkgArchiveObjCode:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" 1
	./run.bash "$(TEST_HC)" "$(BASIC) -fbyte-code-and-object-code"

.PHONY: PkgMultiUnit
PkgMultiUnit:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" 1
	mkdir -p unit2-src/
	mv Local.hs Num.hs Num.hs-boot unit2-src/
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ARGS) -unit @unit1 -unit @unit2
	./PkgBytecode
