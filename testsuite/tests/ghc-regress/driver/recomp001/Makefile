TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Recompilation tests

clean:
	@rm -f *.o *.hi
	@rm -f B.hs C

# 001: removing an export should force a recompile of dependent modules.
# failed in GHC 6.4.

recomp001: clean
	@cp B1.hs B.hs
	@$(TEST_HC) --make -v0 C.hs
	@sleep 1
	@cp B2.hs B.hs
	-@$(TEST_HC) --make -v0 C.hs
