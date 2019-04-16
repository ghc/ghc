TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

unsigned_reloc_macho_x64:
	as -o unsigned_reloc_macho_x64.o unsigned_reloc_macho_x64.s
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RTSOPTS) -v0 --make -no-rtsopts-suggestions -no-hs-main -o runner runner.c
	./runner unsigned_reloc_macho_x64.o getAnswer

section_alignment:
	cc -c -o section_alignment.o section_alignment.c
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RTSOPTS) -v0 --make -no-rtsopts-suggestions -no-hs-main -o runner runner.c
	./runner section_alignment.o isAligned

T2615-prep:
	$(RM) libfoo_T2615.so
	'$(TEST_HC)' $(TEST_HC_OPTS) -fPIC -c libfoo_T2615.c -o libfoo_T2615.o
	'$(TEST_HC)' $(filter-out -rtsopts, $(TEST_HC_OPTS)) -shared -no-auto-link-packages libfoo_T2615.o -o libfoo_T2615.so

#--------------------------------------------------------------------
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

#--------------------------------------------------------------------
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
