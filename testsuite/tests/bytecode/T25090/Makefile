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

DB := -package-db db -package dep
BASIC := $(TEST_HC_OPTS) $(DB) -this-unit-id=pkgdep -v0
BC := -fprefer-byte-code -fbyte-code-and-object-code
ARGS := $(BASIC) $(BC)

T25090_pkg:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" "shared"
	./run.bash "$(TEST_HC)" "$(ARGS) -dynamic"

T25090_pkg_empty:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" "shared-empty"
	./run.bash "$(TEST_HC)" "$(ARGS) -dynamic"

T25090_pkg_nolib:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" "none"
	./run.bash "$(TEST_HC)" "$(ARGS)"

T25090_pkg_obj_code:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" "shared"
	./run.bash "$(TEST_HC)" "$(BASIC) -dynamic -fbyte-code-and-object-code"

T25090_pkg_multi_unit:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" "shared"
	mkdir -p unit2-src/
	mv Local.hs Num.hs Num.hs-boot unit2-src/
	"$(TEST_HC)" $(TEST_HC_OPTS) $(ARGS) -dynamic -unit @unit1 -unit @unit2
	./PkgBytecode

T25090_pkg_archive:
	./prep.bash "$(TEST_HC)" "$(TEST_HC_OPTS)" "$(GHC_PKG)" "archive"
	./run.bash "$(TEST_HC)" "$(ARGS)"
