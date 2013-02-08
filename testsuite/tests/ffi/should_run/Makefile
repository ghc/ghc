TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

ffi018_ghci_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ffi018_ghci_c.c

T1288_ghci_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T1288_ghci_c.c

T2276_ghci_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T2276_ghci_c.c

ffi002_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c ffi002.hs

T5402_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5402.hs

T5594_setup :
	'$(TEST_HC)' $(TEST_HC_OPTS) -c T5594.hs

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

