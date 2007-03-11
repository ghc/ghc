TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# A mulit-module test that made GHC 6.4.1 crash
gadt17:
	@$(RM) gadt17.hi Gadt17_help.hi
	@$(RM) A$(OBJSUFFIX)
	$(TEST_HC) -c Gadt17_help.hs
	$(TEST_HC) -c gadt17.hs

gadt23:
	$(RM) gadt23.hi gadt23.o Gadt23_AST.hi Gadt23_AST.o
	$(TEST_HC) -v0 --make gadt23.hs
	touch gadt23.hs
	$(TEST_HC) -v0 --make gadt23.hs

