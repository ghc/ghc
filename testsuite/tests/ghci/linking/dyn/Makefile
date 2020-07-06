TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

ifeq "$(WINDOWS)" "YES"
DLL = lib$1.dll
else ifeq "$(DARWIN)" "YES"
DLL = lib$1.dylib
else
DLL = lib$1.so
endif

ifeq "$(WINDOWS)" "YES"
EXE = $1.exe
else ifeq "$(DARWIN)" "YES"
EXE = $1
else
EXE = $1
endif

ifeq "$(WINDOWS)" "YES"
CFLAGS =
else
CFLAGS = -fPIC
endif

MY_TEST_HC_OPTS = $(filter-out -rtsopts,$(TEST_HC_OPTS)) $(CFLAGS)

# --------------------------------------------------------------
# Note: libAS.def is not used directly in these tests but is
#       included for completeness. libAS.def is the input to
#       the Microsoft tool "lib.exe" that can be used to
#       create the import libraries using the short format
#       that the `compile_libAS_impl_msvc' target uses.
#
#       See #11072
# --------------------------------------------------------------

.PHONY: load_short_name
load_short_name:
	rm -rf bin_short
	mkdir bin_short
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -odir "bin_short" -shared A.c -o "bin_short/$(call DLL,A)"
	rm -f bin_short/*.a
	echo ":q" | "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -L"./bin_short" -lA

.PHONY: compile_libT1407
compile_libT1407:
	rm -rf T1407dir
	mkdir T1407dir
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -odir "T1407dir" -shared A.c -o "T1407dir/$(call DLL,AS)"
	rm -f T1407dir/libAS*.a

.PHONY: compile_libT10458
compile_libT10458:
	rm -rf T10458dir
	mkdir T10458dir
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -odir "T10458dir" -shared A.c -o "T10458dir/$(call DLL,AS)"
	rm -f T10458dir/libAS*.a

.PHONY: compile_libAB_dep
compile_libAB_dep:
	rm -rf bin_dep
	mkdir bin_dep
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -odir "bin_dep" -shared A.c -o "bin_dep/$(call DLL,A)"
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -odir "bin_dep" -shared B.c -o "bin_dep/$(call DLL,B)" -lA -L"./bin_dep"
	rm -f bin_dep/*.a

.PHONY: compile_libAB_dyn
compile_libAB_dyn:
	rm -rf bin_dyn
	mkdir bin_dyn
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -odir "bin_dyn" -shared A.c -o "bin_dyn/$(call DLL,A)"
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -odir "bin_dyn" -shared B.c -o "bin_dyn/$(call DLL,B)" -lA -L"./bin_dyn"
	rm -f bin_dyn/*.a
	'$(TEST_HC)' $(TEST_HC_OPTS) -ignore-dot-ghci -v0 -o "bin_dyn/$(call EXE,T10955dyn)" -L./bin_dyn -lB -lA T10955dyn.hs -v0
	LD_LIBRARY_PATH=./bin_dyn ./bin_dyn/$(call EXE,T10955dyn)

.PHONY: compile_libAS_impl_gcc
compile_libAS_impl_gcc:
	rm -rf bin_impl_gcc
	mkdir bin_impl_gcc
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -odir "bin_impl_gcc" -shared A.c -o "bin_impl_gcc/$(call DLL,ASimpL)"
	mv bin_impl_gcc/libASimpL.dll.a bin_impl_gcc/libASx.dll.a
	echo "main" | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) T11072.hs -lASx -L./bin_impl_gcc

.PHONY: compile_libAS_impl_msvc
compile_libAS_impl_msvc:
	rm -rf bin_impl_msvc
	mkdir bin_impl_msvc
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -odir "bin_impl_msvc" -shared A.c -o "bin_impl_msvc/$(call DLL,ASimpL)"
	rm -f bin_impl_msvc/libAS*.a
	echo "main" | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) T11072.hs -lAS -L./bin_impl_msvc -L"./$(shell uname -m)"

.PHONY: T1407
T1407:
	cat T1407.script | LD_LIBRARY_PATH=".:$(LD_LIBRARY_PATH)" "$(TEST_HC)" $(TEST_HC_OPTS_INTERACTIVE) -L"T1407dir"

.PHONY: T3242
	echo ":q" | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -lm

.PHONY: T13606
T13606:
	echo ":q" | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -lgcc_s

.PHONY: big-obj
big-obj:
	'$(TEST_CC)' -c -Wa,-mbig-obj big-obj-c.c -o big-obj-c.o
	echo "main" | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) big-obj-c.o big-obj.hs
