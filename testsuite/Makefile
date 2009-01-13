TOP = .
include $(TOP)/mk/boilerplate.mk

boot:
	$(MAKE) -C $(TOP)/timeout all

all:
	$(MAKE) -C $(TOP)/tests/ghc-regress all

clean distclean:
	$(MAKE) -C $(TOP)/timeout $@

