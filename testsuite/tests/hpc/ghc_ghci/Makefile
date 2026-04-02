TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

hpc_ghc_ghci:
	rm -f A.o A.hi
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhpc -c A.hs
	echo b | '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) B.hs

hpc_ghci01:
	HPCTIXFILE=ghc.tix '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -fhpc \
		A.hs B.hs \
		-e ":m B" -e "b"
	"$(HPC)" report ghc.tix
	"$(HPC)" report ghc.tix --per-module
	"$(HPC)" markup ghc.tix

hpc_ghci02:
	'$(TEST_HC)' $(TEST_HC_OPTS) -fhpc -fwrite-byte-code -fprefer-byte-code -fbyte-code -no-link A.hs B.hs
	HPCTIXFILE=ghc.tix '$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -fhpc -fwrite-byte-code -fprefer-byte-code -fbyte-code \
		A.hs B.hs \
		-e ":m B" -e "b"
	"$(HPC)" report ghc.tix
	"$(HPC)" report ghc.tix --per-module
	"$(HPC)" markup ghc.tix

hpc_ghc_ghci_bytecode:
	rm -f ./*.tix
	'$(TEST_HC)' $(TEST_HC_OPTS_INTERACTIVE) -fhpc -fbyte-code-and-object-code -fprefer-byte-code BytecodeMain.hs -e "main" -e ":quit"
	@[ -f .hpc/Main.mix ] || (echo "ERROR: Expected .hpc/Main.mix file not found"; exit 1)
	@set -- ./*.tix; [ -f "$$1" ] || (echo "ERROR: Expected .tix file not found"; exit 1); '$(HPC)' report "$$1" Main > hpc-report.txt
	@grep -F "100% expressions used" hpc-report.txt >/dev/null || (echo "ERROR: Expected full expression coverage in hpc report"; cat hpc-report.txt; exit 1)
