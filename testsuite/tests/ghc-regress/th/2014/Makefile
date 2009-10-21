TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

2014 :
	'$(TEST_HC)' -fforce-recomp -c A.hs-boot
	'$(TEST_HC)' -fforce-recomp -c A.hs
	'$(TEST_HC)' -fforce-recomp -c B.hs
	'$(TEST_HC)' -fforce-recomp -c C.hs -v0
