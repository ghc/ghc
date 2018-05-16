TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

outofmem-prep::
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make -fforce-recomp outofmem.hs -o outofmem

outofmem::
	$(MAKE) -s --no-print-directory outofmem-prep
	@ulimit -m 10000000 2>/dev/null; ./outofmem || echo "exit($$?)"

outofmem2-prep::
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -rtsopts --make -fforce-recomp outofmem2.hs -o outofmem2

outofmem2::
	$(MAKE) -s --no-print-directory outofmem2-prep
	@ulimit -m 1000000 2>/dev/null; ./outofmem2 +RTS -M5m -RTS || echo "exit($$?)"

T2615-prep:
	$(RM) libfoo_T2615.so
	'$(TEST_HC)' $(TEST_HC_OPTS) -fPIC -c libfoo_T2615.c -o libfoo_T2615.o
	'$(TEST_HC)' $(filter-out -rtsopts, $(TEST_HC_OPTS)) -shared -no-auto-link-packages libfoo_T2615.o -o libfoo_T2615.so

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

.PHONY: T9405
T9405:
	@'$(TEST_HC)' $(TEST_HC_OPTS) -ticky -rtsopts T9405.hs; \
		./T9405 +RTS -rT9405.ticky & \
		sleep 0.2; \
		kill -2 $$!; \
		wait $$!; \
		[ -e T9405.ticky ] || echo "Error: Ticky profile doesn't exist"; \
		[ -s T9405.ticky ] || echo "Error: Ticky profile is empty"; \
		echo Ticky-Ticky;

# Naming convention: 'T5423_' obj-way '_' obj-src
# obj-way ::= v | dyn
# obj-src ::= gcc // using __attribute__(constructor)
#           | asm // manually laid out sections
# $(0) = obj-src

define run_T5435_v
$(RM) T5435_load_v_$(1) T5435_v_$(1)$(exeext)
'$(TEST_HC)' $(TEST_HC_OPTS) -optc-D$(HostOS)_HOST_OS -optc-DLOAD_CONSTR=$(2) -v0 -c T5435_$(1).c -o T5435_load_v_$(1).o
'$(TEST_HC)' $(TEST_HC_OPTS) -v0 T5435.hs -osuf main_v_$(1)_o -o T5435_v_$(1)$(exeext)
./T5435_v_$(1) v ./T5435_load_v_$(1).o
endef

define run_T5435_dyn
$(RM) T5435_load_dyn_$(1) T5435_dyn_$(1)$(exeext)
'$(TEST_HC)' $(filter-out -rtsopts, $(TEST_HC_OPTS)) -optc-D$(HostOS)_HOST_OS -v0 -fPIC -shared -c T5435_$(1).c -osuf dyn_$(1)_o -o T5435_load_dyn_$(1)$(dllext)
'$(TEST_HC)' $(TEST_HC_OPTS) -v0 T5435.hs -osuf main_dyn_$(1)_o -o T5435_dyn_$(1)$(exeext)
./T5435_dyn_$(1) dyn ./T5435_load_dyn_$(1)$(dllext)
endef

.PHONY: T5435_v_gcc
T5435_v_gcc :
	$(call run_T5435_v,gcc,0)

.PHONY: T5435_v_asm_a T5435_v_asm_b
T5435_v_asm_a :
	$(call run_T5435_v,asm,0)
T5435_v_asm_b :
	$(call run_T5435_v,asm,1)

.PHONY: T5435_dyn_gcc
T5435_dyn_gcc :
	$(call run_T5435_dyn,gcc)

.PHONY: T5435_dyn_asm
T5435_dyn_asm :
	$(call run_T5435_dyn,asm)

T6006_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T6006.hs

T8124_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T8124.hs

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

.PHONY: T10296a
T10296a:
	$(RM) T10296a_c.o T10296a.o T10296a.hi T10296a_stub.h
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -threaded T10296a.hs T10296a_c.c -o T10296a
	./T10296a +RTS -N2

.PHONY: linker_unload
linker_unload:
	$(RM) Test.o Test.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Test.hs -v0
	# -rtsopts causes a warning
	"$(TEST_HC)" LinkerUnload.hs -package ghc $(filter-out -rtsopts, $(TEST_HC_OPTS)) linker_unload.c -o linker_unload -no-hs-main -optc-Werror
	./linker_unload "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

# -----------------------------------------------------------------------------
# Testing failures in the RTS linker.  We should be able to repeatedly
# load bogus object files of various kinds without crashing and
# without any memory leaks.
#
# Check for memory leaks manually by running e.g.
#
# make linker_error1
# valgrind --leak-check=full --show-reachable=yes ./linker_error1 linker_error1_o.o

# linker_error1: not a valid object file

.PHONY: linker_error1
linker_error1:
	"$(TEST_HC)" -c linker_error.c -o linker_error1.o
	"$(TEST_HC)" linker_error1.o -o linker_error1 -no-hs-main -optc-g -debug -threaded
	./linker_error1 linker_error.c

# linker_error2: the object file has an unknown symbol (fails in
# resolveObjs())

.PHONY: linker_error2
linker_error2:
	"$(TEST_HC)" -c linker_error.c -o linker_error2.o
	"$(TEST_HC)" -c linker_error2.c -o linker_error2_o.o
	"$(TEST_HC)" linker_error2.o -o linker_error2 -no-hs-main -optc-g -debug -threaded
	./linker_error2 linker_error2_o.o

# linker_error3: the object file duplicates an existing symbol (fails
# in loadObj())

.PHONY: linker_error3
linker_error3:
	"$(TEST_HC)" -c linker_error.c -o linker_error3.o
	"$(TEST_HC)" -c linker_error3.c -o linker_error3_o.o
	"$(TEST_HC)" linker_error3.o -o linker_error3 -no-hs-main -optc-g -debug -threaded
	./linker_error3 linker_error3_o.o

 .PHONY: T11788
T11788:
	"$(TEST_HC)" -c T11788.c -o T11788_obj.o
	"$(AR)" rsT libT11788.a T11788_obj.o 2> /dev/null
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) T11788.hs -lT11788 -L"$(PWD)"

 .PHONY: T12497
T12497:
	echo main | "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE)) T12497.hs

.PHONY: T14695
T14695:
	echo ":quit" | LD_LIBRARY_PATH="foo:" "$(TEST_HC)" $(filter-out -rtsopts, $(TEST_HC_OPTS_INTERACTIVE))

.PHONY: InternalCounters
InternalCounters:
	"$(TEST_HC)" +RTS -s --internal-counters -RTS 2>&1 | grep "Internal Counters"
	-"$(TEST_HC)" +RTS -s -RTS 2>&1 | grep "Internal Counters"
