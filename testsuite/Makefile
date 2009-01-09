TOP = .
include $(TOP)/mk/boilerplate.mk

SUBDIRS = timeout utils

all ::
	cd tests/ghc-regress && $(MAKE) $(MFLAGS)

clean distclean:
	$(MAKE) -C timeout $@

