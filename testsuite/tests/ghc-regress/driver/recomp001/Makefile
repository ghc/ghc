TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

# 001: removing an export should force a recompile of dependent modules.
# failed in GHC 6.4.
recomp001:
	@rm -f *.o *.hi
	@cp B1.hs B.hs
	@$(TEST_HC) --make -v0 C.hs
	@sleep 1
	@cp B2.hs B.hs
	-@$(TEST_HC) --make -v0 C.hs
