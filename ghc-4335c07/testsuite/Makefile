TOP = .

# If we're cleaning then we don't want to do all the GHC detection hardwork,
# and we certainly don't want to fail if GHC etc can't be found!
# However, we can't just put this conditional in boilerplate.mk, as
# some of the tests have a "clean" makefile target that relies on GHC_PKG
# being defined.
ifneq "$(MAKECMDGOALS)" "clean"
ifneq "$(MAKECMDGOALS)" "distclean"
ifneq "$(MAKECMDGOALS)" "maintainer-clean"

include $(TOP)/mk/boilerplate.mk

.PHONY: all boot test verbose accept fast slow list_broken

boot:
	$(MAKE) -C $(TOP)/timeout all

all:
	$(MAKE) -C $(TOP)/tests all

test:
	$(MAKE) -C $(TOP)/tests test

verbose:
	$(MAKE) -C $(TOP)/tests verbose

accept:
	$(MAKE) -C $(TOP)/tests accept

fast fasttest:
	$(MAKE) -C $(TOP)/tests fast

slow slowtest:
	$(MAKE) -C $(TOP)/tests slow

list_broken:
	$(MAKE) -C $(TOP)/tests list_broken

endif
endif
endif

clean distclean maintainer-clean:
	$(MAKE) -C $(TOP)/timeout $@
	$(RM) -f mk/*.o
	$(RM) -f mk/*.hi
	$(RM) -f mk/ghcconfig*.mk
	$(RM) -f mk/ghc-config mk/ghc-config.exe
	$(RM) -f driver/*.pyc
