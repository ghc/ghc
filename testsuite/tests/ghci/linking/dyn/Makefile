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


.PHONY: load_short_name
load_short_name:
	rm -rf bin_short
	mkdir bin_short
	gcc -shared A.c -o "bin_short/$(call DLL,A)"
	echo ":q" | "$(TEST_HC)" --interactive -L"$(PWD)/bin_short" -lA -v0

.PHONY: compile_libAS
compile_libAS:
	gcc -shared A.c -o $(call DLL,AS)
