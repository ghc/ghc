TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

tc173:
	$(TEST_HC) -c -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances Tc173a.hs
	$(TEST_HC) -c -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances Tc173b.hs

