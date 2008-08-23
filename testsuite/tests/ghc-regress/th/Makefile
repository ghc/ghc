TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Trac 2386 requires batch-compile not --make
# Very important: without -O 
T2386:
	$(TEST_HC) -v0 -c T2386_Lib.hs -fforce-recomp
	$(TEST_HC) -v0 -c T2386.hs -fforce-recomp

  