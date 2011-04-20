TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Trac 2386 requires batch-compile not --make
# Very important: without -O 
T2386:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -c T2386_Lib.hs -fforce-recomp
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 -c T2386.hs -fforce-recomp


HC_OPTS = -XTemplateHaskell -package template-haskell

TH_spliceE5_prof::
	$(RM) TH_spliceE5_prof*.o TH_spliceE5_prof*.hi TH_spliceE5_prof*.p_o 
	'$(TEST_HC)' $(TEST_HC_OPTS) $(HC_OPTS) --make -v0 TH_spliceE5_prof.hs -c
	'$(TEST_HC)' $(TEST_HC_OPTS) $(HC_OPTS) --make -v0 TH_spliceE5_prof.hs -prof -auto-all -osuf p_o -o $@
	./$@

