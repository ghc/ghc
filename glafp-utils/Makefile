TOP=..
include $(TOP)/mk/gen.mk

# We need to boot msub and mkdependC (in that order), to be sure that
# make depend will succeed in all the other directories.

SUBDIRS = msub mkdependC fastmake lndir ltx mkdirhier runstdtest

boot ::
	$(MAKE) -C msub all
	$(MAKE) -C mkdependC all
	$(MAKE) depend
	$(MAKE) all

include $(TOP)/mk/subdir.mk
