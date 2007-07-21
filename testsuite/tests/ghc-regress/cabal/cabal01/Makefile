TOP=../../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

clean:
	rm -rf setup a.out dist/build/testA
	rm -rf ,tmp dist ,tmp2
	find . -name "*.o" |xargs rm -f
	find . -name "*.hi" |xargs rm -f
	rm -fr install-tmp
	rm -fr install
	rm -f .setup-config .installed-pkg-config
	-$(GHC_PKG_INPLACE) unregister test

# We use the global package database as there's no easy way to tell
# ghc-pkg (via Cabal) to use one in ., and the global one at least
# won't affect the installed GHC and is more likely to work

cabal01:
	$(MAKE) clean
	$(TEST_HC) --make -o setup Setup.lhs
#	unregister if pakage 'test' already exists
	if $(GHC_PKG_INPLACE) --user list | grep test-; then \
	  $(GHC_PKG_INPLACE) --user unregister test; \
	fi

	./setup configure -v0 --prefix=$(shell $(TOP)/utils/pwd forwardslash)/install --with-compiler=$(TEST_HC) --with-hc-pkg=$(GHC_PKG_INPLACE) $(PROF)
	./setup build -v0
	./setup copy -v0
	ls install
	rm -r install
#	install w/ register!
	./setup install -v0
	ls install
	./setup sdist -v0
	ls dist
	$(GHC_PKG_INPLACE) unregister test
