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

.PHONY: load_short_name
load_short_name:
	rm -rf bin_short
	mkdir bin_short
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -odir "bin_short" -shared A.c -o "bin_short/$(call DLL,A)"
	rm -f bin_short/*.a
	echo ":q" | "$(TEST_HC)" --interactive -L"./bin_short" -lA -v0

.PHONY: compile_libAS
compile_libAS:
	'$(TEST_HC)' $(MY_TEST_HC_OPTS) -shared A.c -o $(call DLL,AS)
	rm -f libAS*.a

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

.PHONY: T1407
T1407:
	cat T1407.script | LD_LIBRARY_PATH=. "$(TEST_HC)" $(TEST_HC_OPTS) -ignore-dot-ghci -v0 --interactive -L.

.PHONY: T3242
	echo ":q" | '$(TEST_HC)' $(TEST_HC_OPTS) --interactive -ignore-dot-ghci -v0 -lm
