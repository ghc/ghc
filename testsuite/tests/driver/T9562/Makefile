TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T9562:
	rm -f *.o *.hi *.o-boot *.hi-boot Main
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -c A.hs
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -c B.hs-boot
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -c C.hs
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -c B.hs
	'$(TEST_HC)' $(EXTRA_HC_OPTS) -c D.hs
	! ('$(TEST_HC)' $(EXTRA_HC_OPTS) Main.hs && ./Main)
