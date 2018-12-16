TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# pass -dead_strip to the linker.
HCFLAGS = -O2 -fllvm
HCINC = $(TOP)/../includes

.PHONY: subsections_via_symbols
subsections_via_symbols:
	'$(TEST_HC)' -o SubsectionsViaSymbols.o SubsectionsViaSymbols.hs $(HCFLAGS) -staticlib
	'$(TEST_HC)' -o subsections_via_symbols SubsectionsViaSymbols subsections_via_symbols.m $(HCFLAGS) -optl -dead_strip -no-hs-main
	./subsections_via_symbols
