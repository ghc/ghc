TOP=..
include $(TOP)/mk/boilerplate.mk

ifeq "$(DOING_BIN_DIST)" "YES"
# We're doing a binary-dist, descend into a subset of the dirs.
SUBDIRS = mkdirhier hp2ps parallel unlit
else
ifeq "$(BootingFromHc)" "YES"
SUBDIRS = mkdependC mkdirhier runstdtest genapply genprimopcode unlit
else
SUBDIRS = mkdependC mkdirhier runstdtest hp2ps \
	  parallel unlit genprimopcode genapply
endif
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

WITH_BOOTSTRAPPING_COMPILER = installPackage ghc-pkg hsc2hs hpc

WITH_STAGE1 = installPackage ghc-pkg hasktags runghc hpc pwd
ifneq "$(NO_INSTALL_HSC2HS)" "YES"
WITH_STAGE1 += hsc2hs
endif

# sort removes duplicates - we don't actually care about the order
WITH_EITHER = $(sort $(WITH_BOOTSTRAPPING_COMPILER) $(WITH_STAGE1))

binary-dist: $(foreach P,$(WITH_STAGE1),binary-dist.$P)
	echo $(WHERE_AM_I)/Makefile >> $(BIN_DIST_LIST)
	set -e; for d in $(SUBDIRS); do $(MAKE) -C $$d binary-dist WHERE_AM_I=$(WHERE_AM_I)/$$d; done

clean distclean:: $(foreach P,$(WITH_EITHER),clean.$P)

with-bootstrapping-compiler: \
    $(foreach P,$(WITH_BOOTSTRAPPING_COMPILER),with-bootstrapping-compiler.$P)

with-stage-1: $(foreach P,$(WITH_STAGE1),with-stage-1.$P)

install:: $(foreach P,$(WITH_STAGE1),install.$P)

$(foreach P,$(WITH_EITHER),clean.$P): \
clean.%:
	$(MAKE) -C $* clean

$(foreach P,$(WITH_BOOTSTRAPPING_COMPILER),with-bootstrapping-compiler.$P): \
with-bootstrapping-compiler.%:
	$(MAKE) -C $* with-bootstrapping-compiler

$(foreach P,$(WITH_STAGE1),with-stage-1.$P): \
with-stage-1.%:
	$(MAKE) -C $* with-stage-1

$(foreach P,$(WITH_STAGE1),install.$P): \
install.%:
	$(MAKE) -C $* install

$(foreach P,$(WITH_STAGE1),binary-dist.$P): \
binary-dist.%:
	$(MAKE) -C $* binary-dist WHERE_AM_I=$(WHERE_AM_I)/$*

