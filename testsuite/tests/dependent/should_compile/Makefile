TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T13938:
	$(RM) T13938a.hi T13938a.o T13938.hi T13938.o
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T13938a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -O -c T13938.hs

source_call_hc = '$(TEST_HC)' $(TEST_HC_OPTS) -v0 -fno-force-recomp -fomit-interface-pragmas -fno-write-if-simplified-core -i -iinterfaces -outputdir interfaces

NativeSourceCallIface:
	mkdir -p interfaces changed
	$(source_call_hc) -fno-code -fwrite-interface -c ClosedTableLibrary.hs
	$(source_call_hc) -c NativeSourceCallIface.hs
	sed 's/flipBit False = True/flipBit False = False/' ClosedTableLibrary.hs > changed/ClosedTableLibrary.hs
	$(source_call_hc) -fno-code -fwrite-interface -fforce-recomp -c changed/ClosedTableLibrary.hs
	! $(source_call_hc) -c NativeSourceCallIface.hs 2>changed.stderr
	grep -q 'GHC-83865' changed.stderr
	$(source_call_hc) -fno-code -fwrite-interface -c NativeMethodAliases.hs
	$(source_call_hc) -c NativeMethodAliasClient.hs
	sed 's/forward = turn/forward = stay/' NativeMethodAliases.hs > changed/NativeMethodAliases.hs
	$(source_call_hc) -fno-code -fwrite-interface -fforce-recomp -c changed/NativeMethodAliases.hs
	! $(source_call_hc) -c NativeMethodAliasClient.hs 2>changed-method.stderr
	grep -q 'GHC-83865' changed-method.stderr
	grep -q 'forwardProof' changed-method.stderr
	$(source_call_hc) -fno-code -fwrite-interface -c PackedVecLibrary.hs
	$(source_call_hc) -c NativeLiteralClient.hs
	sed 's/n == 0 = Z/n == 0 = S Z/' PackedVecLibrary.hs > changed/PackedVecLibrary.hs
	$(source_call_hc) -fno-code -fwrite-interface -fforce-recomp -c changed/PackedVecLibrary.hs
	! $(source_call_hc) -c NativeLiteralClient.hs 2>changed-literal.stderr
	grep -q 'has no checked zero and successor equations' changed-literal.stderr
