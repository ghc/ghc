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
#	unregister if pakage 'test' already exists
	if $(GHC_PKG_INPLACE) --user list | grep test-; then \
	  $(GHC_PKG_INPLACE) --user unregister test; \
	fi

# We use the global package database as there's no easy way to tell
# ghc-pkg (via Cabal) to use one in ., and the global one at least
# won't affect the installed GHC and is more likely to work

cabal01:
	$(MAKE) clean
	$(TEST_HC) --make -o setup Setup.lhs -v0

	./setup configure -v0 --prefix=$(shell $(TOP)/utils/pwd forwardslash)/install --with-compiler=$(TEST_HC) --with-hc-pkg=$(GHC_PKG_INPLACE) $(PROF)
	./setup build -v0
	./setup copy -v0
	echo install1:
	ls -1 install
	rm -r install
#	install w/ register!
	./setup install -v0
	echo install2:
	ls -1 install
	./setup sdist -v0
	echo dist:
	ls dist
	$(GHC_PKG_INPLACE) unregister test
	if [ "$(CLEANUP)" != "" ]; then $(MAKE) clean; fi

