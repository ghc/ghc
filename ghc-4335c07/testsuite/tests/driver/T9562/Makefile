TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

T9562:
	rm -f *.o *.hi *.o-boot *.hi-boot Main
	'$(TEST_HC)' -c A.hs
	'$(TEST_HC)' -c B.hs-boot
	'$(TEST_HC)' -c C.hs
	'$(TEST_HC)' -c B.hs
	'$(TEST_HC)' -c D.hs
	! ('$(TEST_HC)' Main.hs && ./Main)
