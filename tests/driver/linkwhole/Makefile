TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test for -fwhole-archive-hs-libs

ifeq "$(HostOS)" "darwin"
NO_GC_SECTIONS=
else
NO_GC_SECTIONS=-optl-Wl,--no-gc-sections
endif

linkwhole:
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Types.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Main.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -o host Main.o Types.o -fwhole-archive-hs-libs -package ghci -rdynamic $(NO_GC_SECTIONS)
	"$(TEST_HC)" $(TEST_HC_OPTS) -c MyCode.hs
	"$(TEST_HC)" $(TEST_HC_OPTS) -c Handles.hs
	ld -r -o lib.so MyCode.o Handles.o
	./host lib.so
