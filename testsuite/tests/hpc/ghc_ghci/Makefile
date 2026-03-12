TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

hpc_ghc_ghci:
	rm -f A.o A.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhpc -c A.hs
	echo b | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) B.hs

hpc_ghc_ghci_bytecode:
	rm -f ./*.tix
	printf "main\n:quit\n" | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -fhpc -fbyte-code-and-object-code -fprefer-byte-code BytecodeMain.hs
	@[ -f .hpc/Main.mix ] || (echo "ERROR: Expected .hpc/Main.mix file not found"; exit 1)
	@set -- ./*.tix; [ -f "$$1" ] || (echo "ERROR: Expected .tix file not found"; exit 1); '$(HPC)' report "$$1" Main > hpc-report.txt
	@grep -F "100% expressions used" hpc-report.txt >/dev/null || (echo "ERROR: Expected full expression coverage in hpc report"; cat hpc-report.txt; exit 1)
