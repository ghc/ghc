TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

unsigned_reloc_macho_x64:
	as -o unsigned_reloc_macho_x64.o unsigned_reloc_macho_x64.s
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RTSOPTS) -v0 --make -no-rtsopts-suggestions -no-hs-main -o runner runner.c
	./runner unsigned_reloc_macho_x64.o getAnswer

section_alignment:
	'$(TEST_CC)' $(TEST_CC_OPTS) -c -o section_alignment.o section_alignment.c
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RTSOPTS) -v0 --make -no-rtsopts-suggestions -no-hs-main -o runner runner.c
	./runner section_alignment.o isAligned

T23066:
	'$(TEST_CC)' $(TEST_CC_OPTS) -c -o T23066_c.o T23066_c.c
	'$(TEST_HC)' $(TEST_HC_OPTS_NO_RTSOPTS) -v0 --make -no-rtsopts-suggestions -no-hs-main -o runner runner.c -static
	./runner T23066_c.o isAligned

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

.PHONY: linker_unload_native
linker_unload_native:
	$(RM) Test.o Test.hi Test.a Test.so Test2.so
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Test.hs -v0 -dynamic -fPIC -o Test.a
	# only libraries without DT_NEEDED are supported
	"$(TEST_CC)" -shared -Wl,-Bsymbolic -nostdlib -o Test.so -Wl,-nostdlib \
		-Wl,--whole-archive Test.a
	cp Test.so Test2.so

	# -rtsopts causes a warning
	"$(TEST_HC)" LinkerUnload.hs -optl-Wl,--export-dynamic -package ghc \
		$(filter-out -rtsopts, $(TEST_HC_OPTS)) linker_unload_native.c \
		-o linker_unload_native -no-hs-main -optc-Werror
	./linker_unload_native \
		"`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"

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

.PHONY: T7072
T7072:
	"$(TEST_HC)" load-object.c -o load-object -no-hs-main -debug
	"$(TEST_HC)" -c T7072.c -o T7072.o
	./load-object T7072.o

.PHONY: T20494
T20494:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c T20494-obj.c -o T20494-obj.o
	"$(TEST_HC)" -v0 T20494.hs
	./T20494 T20494-obj.o

.PHONY: T20918
T20918:
	"$(TEST_HC)" -c T20918_v.cc -o T20918_v.o
	echo hello | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) T20918_v.o T20918.hs

.PHONY: T21618
T21618:
	"$(TEST_HC)" -c T21618_c.c -o T21618_c.o
	echo main | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) T21618_c.o T21618.hs

.PHONY: reloc-none
reloc-none:
	"$(TEST_HC)" load-object.c -o load-object -no-hs-main -debug
	"$(TEST_HC)" -c reloc-none.c -o reloc-none.o
	./load-object reloc-none.o
