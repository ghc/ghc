TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

tc173:
	$(RM) Tc173a.o Tc173a.hi Tc173b.o Tc173b.hi
	$(TEST_HC) -c -fglasgow-exts -XUndecidableInstances -XOverlappingInstances Tc173a.hs
	$(TEST_HC) -c -fglasgow-exts -XUndecidableInstances -XOverlappingInstances Tc173b.hs

