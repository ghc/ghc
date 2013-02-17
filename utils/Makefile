TOP=..
include $(TOP)/mk/boilerplate.mk

ifeq "$(DOING_BIN_DIST)" "YES"
# We're doing a binary-dist, descend into a subset of the dirs.
SUBDIRS = mkdirhier hp2ps parallel unlit
else
SUBDIRS = mkdependC mkdirhier runstdtest hp2ps \
	  parallel unlit genprimopcode genapply
#ifneq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
## lndir doesn't build on Windows
#SUBDIRS += lndir
#endif
endif

ifeq "$(TARGETPLATFORM)" "i386-unknown-mingw32"
SUBDIRS += touchy
endif

# XXX pwd and lndir building disabled for now

# Utils that we don't build by default:
#	nofib-analyse

# Utils that are old and/or bitrotted:
#	stat2resid
#	debugNCG
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
boot ::
	$(MAKE) -C genprimopcode

############################################

# The utils may be built with the bootstrapping compiler, for use during
# the build, or with the stage2 compiler, for installing. Some of them
# are built with both; we can't install the utils built with the
# bootstrapping compiler as they may use different versions of C
# libraries. The reason we use stage2 rather than stage1 is that some
# utils, e.g. haddock, need the GHC API package.

WITH_BOOTSTRAPPING_COMPILER = installPackage ghc-pkg hsc2hs hpc

WITH_STAGE2 = installPackage ghc-pkg runghc hpc pwd haddock
ifneq "$(NO_INSTALL_HSC2HS)" "YES"
WITH_STAGE2 += hsc2hs
endif

# sort removes duplicates - we don't actually care about the order
WITH_EITHER = $(sort $(WITH_BOOTSTRAPPING_COMPILER) $(WITH_STAGE2))

# We need to build pwd with stage 2, as it goes in bindists, but we
# don't actually want to install it. Likewise the installPackage
# program.
DO_NOT_INSTALL = pwd installPackage

binary-dist: $(foreach P,$(WITH_STAGE2),binary-dist.$P)
ifeq "$(WHERE_AM_I)" ""
	echo "I don't know where I am" >&2
	exit 1
endif
	echo $(WHERE_AM_I)/Makefile >> $(BIN_DIST_LIST)
	set -e; for d in $(SUBDIRS); do $(MAKE) -C $$d binary-dist WHERE_AM_I=$(WHERE_AM_I)/$$d; done

clean:: $(foreach P,$(WITH_EITHER),clean.$P)

distclean:: $(foreach P,$(WITH_EITHER),distclean.$P)

with-bootstrapping-compiler: \
    $(foreach P,$(WITH_BOOTSTRAPPING_COMPILER),with-bootstrapping-compiler.$P)

with-stage-2: $(foreach P,$(WITH_STAGE2),with-stage-2.$P)
	$(MAKE) -C haddock install-inplace

install:: $(foreach P,$(filter-out $(DO_NOT_INSTALL),$(WITH_STAGE2)),install.$P)

$(foreach P,$(WITH_EITHER),clean.$P): \
clean.%:
	$(MAKE) -C $* clean

$(foreach P,$(WITH_EITHER),distclean.$P): \
distclean.%:
	$(MAKE) -C $* distclean

$(foreach P,$(WITH_BOOTSTRAPPING_COMPILER),with-bootstrapping-compiler.$P): \
with-bootstrapping-compiler.%:
	$(MAKE) -C $* with-bootstrapping-compiler

$(foreach P,$(WITH_STAGE2),with-stage-2.$P): \
with-stage-2.%:
	$(MAKE) -C $* with-stage-2

$(foreach P,$(WITH_STAGE2),install.$P): \
install.%:
	$(MAKE) -C $* install

$(foreach P,$(WITH_STAGE2),binary-dist.$P): \
binary-dist.%:
	$(MAKE) -C $* binary-dist WHERE_AM_I=$(WHERE_AM_I)/$*

