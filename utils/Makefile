TOP=..
include $(TOP)/mk/boilerplate.mk

ifneq "$(BIN_DIST_NAME)" ""
# We're doing a binary-dist, descend into a subset of the dirs.
SUBDIRS = hp2ps unlit
else
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = mkdependC mkdirhier runstdtest genapply genprimopcode ghc-pkg unlit
else
SUBDIRS = mkdependC mkdirhier runstdtest ghc-pkg hasktags hp2ps hsc2hs \
	  parallel prof unlit genprimopcode genapply runghc
endif
endif

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SUBDIRS += touchy
endif

ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
# lndir doesn't build on Windows
SUBDIRS += lndir
endif

# Utils that we don't build by default:
#	nofib-analyse

# Utils that are old and/or bitrotted:
#	stat2resid
#	debugNCG
#	ext-core
#	genargs
#	heap-view
#	pvm
#	verbatim
#	ltx
#	hstags

# "heap-view" is not in the list because (a) it requires
#  a Haskell compiler (which you may not have yet), and (b) you are
#  unlikely to want it desperately.  It is easy to build once you have
#  a Haskell compiler and if you want it.

include $(TOP)/mk/target.mk

# genprimopcode is needed to boot in ghc/compiler...
ifneq "$(BootingFromHc)" "YES"
boot ::
	$(MAKE) -C genprimopcode
endif

