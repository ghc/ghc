TOP = .
include $(TOP)/mk/boilerplate.mk

all:
	cd tests/ghc-regress && $(MAKE) $(MFLAGS)

clean distclean:
	$(MAKE) -C timeout $@

boot:
	cd $(TOP)/timeout && $(MAKE) $(MFLAGS) all
