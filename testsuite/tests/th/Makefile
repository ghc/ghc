TOP=../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Trac 2386 requires batch-compile not --make
# Very important: without -O 
T2386:
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -v0 -c T2386_Lib.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -v0 -c T2386.hs

T7445:
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -v0 -c T7445a.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) -v0 -c T7445.hs

HC_OPTS = -XTemplateHaskell -package template-haskell

TH_spliceE5_prof::
	$(RM) TH_spliceE5_prof*.o TH_spliceE5_prof*.hi TH_spliceE5_prof*.p.o 
	'$(TEST_HC)' $(TEST_HC_OPTS) $(HC_OPTS) --make -v0 TH_spliceE5_prof.hs -c
	'$(TEST_HC)' $(TEST_HC_OPTS) $(HC_OPTS) --make -v0 TH_spliceE5_prof.hs -prof -auto-all -osuf p.o -o $@
	./$@

.PHONY: TH_Depends
TH_Depends:
	$(RM) TH_Depends_external.txt
	$(RM) TH_Depends TH_Depends.exe
	$(RM) TH_Depends.o TH_Depends.hi
	$(RM) TH_Depends_External.o TH_Depends_External.hi
	echo "first run" > TH_Depends_external.txt
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -v0 TH_Depends
	./TH_Depends
	sleep 2
	echo "second run" > TH_Depends_external.txt
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --make -v0 TH_Depends
	./TH_Depends


T8333:
	'$(TEST_HC)' $(TEST_HC_OPTS) $(ghcThWayFlags) --interactive -v0 T8333.hs < /dev/null
