TOP = .
include $(TOP)/mk/boilerplate.mk

SUBDIRS = timeout utils

CLEAN_FILES += mk/wordsize.mk

all ::
	cd tests/ghc-regress && $(MAKE) $(MFLAGS)

include $(TOP)/mk/target.mk
