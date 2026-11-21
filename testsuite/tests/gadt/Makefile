TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

.PHONY: gadt17 gadt23

# A multi-module test that made GHC 6.4.1 crash
gadt17:
	@$(RM) gadt17.hi Gadt17_help.hi
	@$(RM) A$(OBJSUFFIX)
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Gadt17_help.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c gadt17.hs

T7321:
	@$(RM) T7321a.hi T7321.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7321a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T7321.hs

gadt23:
	$(RM) gadt23 gadt23.hi gadt23.o Gadt23_AST.hi Gadt23_AST.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make gadt23.hs
	# Append a blank line to gadt23.hs to trigger a recompile
	echo >> gadt23.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make gadt23.hs


.PHONY: TerminalGadtContext
TerminalGadtContext:
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -fomit-interface-pragmas -fno-write-if-simplified-core -outputdir . -c LabelledValue.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -fomit-interface-pragmas -fno-write-if-simplified-core -outputdir . -c TerminalGadtContextProvider.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -i -i. -outputdir . -c TerminalGadtContext.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 Main.o TerminalGadtContextProvider.o LabelledValue.o -o TerminalGadtContext$(exeext)
	@./TerminalGadtContext$(exeext)

.PHONY: GadtTrailingContext
GadtTrailingContext:
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -fomit-interface-pragmas -fno-write-if-simplified-core -outputdir . -c GadtTrailingContextProvider.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -i -i. -outputdir . -c GadtTrailingContext.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 Main.o GadtTrailingContextProvider.o -o GadtTrailingContext$(exeext)
	@./GadtTrailingContext$(exeext)

.PHONY: GadtInteriorContext
GadtInteriorContext:
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -fomit-interface-pragmas -fno-write-if-simplified-core -outputdir . -c GadtInteriorContextProvider.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -i -i. -outputdir . -c GadtInteriorContext.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 Main.o GadtInteriorContextProvider.o -o GadtInteriorContext$(exeext)
	@./GadtInteriorContext$(exeext)

.PHONY: GadtInteriorForAll
GadtInteriorForAll:
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -fomit-interface-pragmas -fno-write-if-simplified-core -outputdir . -c GadtInteriorForAllProvider.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -i -i. -outputdir . -c GadtInteriorForAll.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 Main.o GadtInteriorForAllProvider.o -o GadtInteriorForAll$(exeext)
	@./GadtInteriorForAll$(exeext)

.PHONY: GadtRepeatedForAll
GadtRepeatedForAll:
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -fomit-interface-pragmas -fno-write-if-simplified-core -outputdir . -c GadtRepeatedForAllProvider.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -i -i. -outputdir . -c GadtRepeatedForAll.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 Main.o GadtRepeatedForAllProvider.o -o GadtRepeatedForAll$(exeext)
	@./GadtRepeatedForAll$(exeext)

.PHONY: GadtOrderedTelescope
GadtOrderedTelescope:
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -fomit-interface-pragmas -fno-write-if-simplified-core -outputdir . -c GadtOrderedTelescopeProvider.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -O2 -dcore-lint -dstg-lint -i -i. -outputdir . -c GadtOrderedTelescope.hs
	@'$(TEST_HC)' $(TEST_HC_OPTS) -v0 Main.o GadtOrderedTelescopeProvider.o -o GadtOrderedTelescope$(exeext)
	@./GadtOrderedTelescope$(exeext)
