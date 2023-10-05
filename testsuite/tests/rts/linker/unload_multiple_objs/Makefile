TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: linker_unload_multiple_objs
linker_unload_multiple_objs:
	$(RM) A.o  B.o  C.o  D.o
	$(RM) A.hi B.hi C.hi D.hi
	"$(TEST_HC)" $(TEST_HC_OPTS) -c A.hs -v0
	"$(TEST_HC)" $(TEST_HC_OPTS) -c B.hs -v0
	"$(TEST_HC)" $(TEST_HC_OPTS) -c C.hs -v0
	"$(TEST_HC)" $(TEST_HC_OPTS) -c D.hs -v0
	# -rtsopts causes a warning
	"$(TEST_HC)" LinkerUnload.hs -package ghc $(filter-out -rtsopts, $(TEST_HC_OPTS)) linker_unload_multiple_objs.c -o linker_unload_multiple_objs -no-hs-main -optc-Werror
	./linker_unload_multiple_objs "`'$(TEST_HC)' $(TEST_HC_OPTS) --print-libdir | tr -d '\r'`"


