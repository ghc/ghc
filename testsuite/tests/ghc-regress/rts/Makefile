TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

outofmem-prep::
	$(TEST_HC) --make -fforce-recomp outofmem.hs -o outofmem

outofmem::
	@$(MAKE) outofmem-prep >prep.out 2>prep.out
	@ulimit -v 100000 2>/dev/null; ./outofmem || echo "exit($$?)"

outofmem2-prep::
	$(TEST_HC) --make -fforce-recomp outofmem2.hs -o outofmem2

outofmem2::
	@$(MAKE) outofmem2-prep >prep.out 2>prep.out
	@ulimit -v 100000 2>/dev/null; ./outofmem2 +RTS -M5m -RTS || echo "exit($$?)"
