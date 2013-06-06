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

boot:
	$(MAKE) -C $(TOP)/timeout all

all:
	$(MAKE) -C $(TOP)/tests all

fast:
	$(MAKE) -C $(TOP)/tests fast

endif
endif
endif

clean distclean maintainer-clean:
	$(MAKE) -C $(TOP)/timeout $@
	$(RM) -f mk/*.o mk/*.hi mk/ghc-config mk/ghc-config.exe mk/ghcconfig*.mk
