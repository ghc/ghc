TOP=..
include $(TOP)/mk/boilerplate.mk

ifneq "$(BIN_DIST_NAME)" ""
# We're doing a binary-dist, descend into a subset of the dirs.
SUBDIRS = hp2ps stat2resid unlit
else
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = genapply genprimopcode ghc-pkg unlit
else
SUBDIRS = hasktags ghc-pkg hp2ps hsc2hs parallel stat2resid prof unlit genprimopcode genapply
endif
endif

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SUBDIRS += touchy
endif

# hstags died when the new parser was introduced.
#	  hstags	\

# "heap-view" is not in the list because (a) it requires
#  a Haskell compiler (which you may not have yet), and (b) you are
#  unlikely to want it desperately.  It is easy to build once you have
#  a Haskell compiler and if you want it.

include $(TOP)/mk/target.mk
