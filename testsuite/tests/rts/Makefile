TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

outofmem-prep::
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make -fforce-recomp outofmem.hs -o outofmem

outofmem::
	@$(MAKE) outofmem-prep
	@ulimit -v 10000000 2>/dev/null; ./outofmem || echo "exit($$?)"

outofmem2-prep::
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts --make -fforce-recomp outofmem2.hs -o outofmem2

outofmem2::
	@$(MAKE) outofmem2-prep
	@ulimit -v 1000000 2>/dev/null; ./outofmem2 +RTS -M5m -RTS || echo "exit($$?)"

T2615-prep:
	$(RM) libfoo_T2615.so
	'$(TEST_HC)' $(TEST_HC_OPTS) -fPIC -c libfoo_T2615.c -o libfoo_T2615.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -shared -no-auto-link-packages libfoo_T2615.o -o libfoo_T2615.so

.PHONY: T4059
T4059:
	$(RM) T4059_c.o T4059.o T4059.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T4059 T4059_c.c
	./T4059

exec_signals-prep:
	$(CC) -o exec_signals_child exec_signals_child.c
	$(CC) -o exec_signals_prepare exec_signals_prepare.c

.PHONY: T4850
T4850:
	$(RM) T4850.o T4850.hi T4850$(exeext)
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -rtsopts -debug -threaded --make T4850
	./T4850 +RTS -s 2>&1 | grep TASKS | sed 's/^ *TASKS: *\([0-9]*\).*$$/\1/'

.PHONY: T5423
T5423:
	$(RM) T5423_cmm.o T5423.o T5423.hi T5423$(exeext)
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c T5423_cmm.cmm
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 -c T5423.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -v0 T5423.o T5423_cmm.o -o T5423$(exeext)
	./T5423

T6006_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T6006.hs

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
T7037_CONST = const
else
T7037_CONST =
endif
.PHONY: T7037
T7037:
	$(RM) 7037.o 7037.hi 7037$(exeext)
	"$(TEST_HC)" $(TEST_HC_OPTS) T7037.hs -v0
	"$(TEST_HC)" -optc-DT7037_CONST=$(T7037_CONST) $(filter-out -rtsopts, $(TEST_HC_OPTS)) T7037_main.c -v0 -o T7037_main -no-hs-main
	./T7037_main

T7040_ghci_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghciWayFlags) -c T7040_ghci_c.c

LOCAL_GHC_PKG = '$(GHC_PKG)' --no-user-package-db

BASE_DIR = $(shell $(LOCAL_GHC_PKG) field base library-dirs | sed 's/^.*: *//')
BASE_LIB = $(shell $(LOCAL_GHC_PKG) field base hs-libraries | sed 's/^.*: *//')
GHC_PRIM_DIR = $(shell $(LOCAL_GHC_PKG) field ghc-prim library-dirs | sed 's/^.*: *//')
GHC_PRIM_LIB = $(shell $(LOCAL_GHC_PKG) field ghc-prim hs-libraries | sed 's/^.*: *//')
INTEGER_GMP_DIR = $(shell $(LOCAL_GHC_PKG) field integer-gmp library-dirs | sed 's/^.*: *//')
INTEGER_GMP_LIB = $(shell $(LOCAL_GHC_PKG) field integer-gmp hs-libraries | sed 's/^.*: *//')
$(warning xxx$(BASE_DIR)xxx)

BASE        = $(BASE_DIR)/lib$(BASE_LIB).a
GHC_PRIM    = $(GHC_PRIM_DIR)/lib$(GHC_PRIM_LIB).a
INTEGER_GMP = $(INTEGER_GMP_DIR)/lib$(INTEGER_GMP_LIB).a

.PHONY: linker_unload
linker_unload:
	$(RM) Test.o Test.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Test.hs -v0
	# -rtsopts causes a warning
	"$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS)) linker_unload.c -o linker_unload -no-hs-main
	./linker_unload $(BASE) $(GHC_PRIM) $(INTEGER_GMP)
