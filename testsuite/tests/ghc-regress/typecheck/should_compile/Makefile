TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

tc173:
	$(TEST_HC) -c -fglasgow-exts -XUndecidableInstances -XOverlappingInstances Tc173a.hs
	$(TEST_HC) -c -fglasgow-exts -XUndecidableInstances -XOverlappingInstances Tc173b.hs

