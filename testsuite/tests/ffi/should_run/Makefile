TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

ffi018_ghci_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghciWayFlags) -c ffi018_ghci_c.c -o ffi018_ghci_c.o

T1288_ghci_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghciWayFlags) -c T1288_ghci_c.c -o T1288_ghci_c.o

T2276_ghci_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghciWayFlags) -c T2276_ghci_c.c -o T2276_ghci_c.o

ffi002_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ffi002.hs

T5402_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5402.hs

T5594_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5594.hs

ffi023_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ffi023.hs

rts_clearMemory_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c rts_clearMemory.hs

.PHONY: Capi_Ctype_001
Capi_Ctype_001:
	'$(HSC2HS)' Capi_Ctype_A_001.hsc
	'$(HSC2HS)' Capi_Ctype_001.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c capi_ctype_001_c.c
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Capi_Ctype_A_001.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Capi_Ctype_001.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) capi_ctype_001_c.o Capi_Ctype_A_001.o Capi_Ctype_001.o -o Capi_Ctype_001
	./Capi_Ctype_001

.PHONY: Capi_Ctype_002
Capi_Ctype_002:
	'$(HSC2HS)' Capi_Ctype_A_002.hsc
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Capi_Ctype_A_002.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) -c Capi_Ctype_002.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) Capi_Ctype_A_002.o Capi_Ctype_002.o -o Capi_Ctype_002
	./Capi_Ctype_002

.PHONY: T15933
T15933:
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T15933_c.c
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T15933.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) T15933_c.o T15933.o -o T15933
	./T15933

.PHONY: T22159
T22159:
	C_INCLUDE_PATH=/clang64/include '$(TEST_HC)' $(TEST_HC_OPTS) -c T22159.hs
	C_INCLUDE_PATH=/clang64/include '$(TEST_HC)' $(TEST_HC_OPTS) -c T22159_c.c
	C_INCLUDE_PATH=/clang64/include '$(TEST_HC)' $(TEST_HC_OPTS) T22159.o T22159_c.o -o T22159
	./T22159
