TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T14482:
	rm -f *.o *.hi *.o-boot *.hi-boot C result
	'$(TEST_HC)' -M C.hs -dep-suffix "p_" -dep-suffix "q_" -dep-makefile result
	cat result
