# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/architecture
#      https://gitlab.haskell.org/ghc/ghc/wikis/building/modifying
#
# -----------------------------------------------------------------------------

# We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
# gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
# That's because the doc/ directory contents are under the GFDL,
# which causes problems for Debian.

ifneq "$(BINDIST)" "YES"
GMP_TARBALL := $(wildcard libraries/ghc-bignum/gmp/gmp-tarballs/gmp*.tar.bz2)
GMP_DIR := $(patsubst libraries/ghc-bignum/gmp/gmp-tarballs/%-nodoc.tar.bz2,%,$(GMP_TARBALL))

ifeq "$(GMP_TARBALL)" ""
$(error "GMP tarball is missing; you may need to run 'git submodule update --init'.")
endif
endif

ifneq "$(NO_CLEAN_GMP)" "YES"
$(eval $(call clean-target,gmp,,\
  libraries/ghc-bignum/include/ghc-gmp.h \
  libraries/ghc-bignum/gmp/libgmp.a \
  libraries/ghc-bignum/gmp/gmp.h \
  libraries/ghc-bignum/gmp/gmpbuild \
  libraries/ghc-bignum/gmp/$(GMP_DIR)))

clean : clean_gmp
.PHONY: clean_gmp
clean_gmp:
	$(call removeTrees,libraries/ghc-bignum/gmp/objs)
	$(call removeTrees,libraries/ghc-bignum/gmp/gmpbuild)
endif

ifeq "$(GMP_PREFER_FRAMEWORK)" "YES"
libraries/ghc-bignum_CONFIGURE_OPTS += --with-gmp-framework-preferred
endif

ifneq "$(CLEANING)" "YES"
# Hack. The file config.mk doesn't exist yet after running ./configure in
# the toplevel (ghc) directory. To let some toplevel make commands such as
# sdist go through, right after ./configure, don't consider this an error.
-include libraries/ghc-bignum/dist-install/build/config.mk
endif

gmp_CC_OPTS += $(addprefix -I,$(GMP_INCLUDE_DIRS))
gmp_LD_OPTS += $(addprefix -L,$(GMP_LIB_DIRS))

# Compile GMP only if we don't have it already
#
# We use GMP's own configuration stuff, because it's all rather hairy
# and not worth re-implementing in our Makefile framework.

ifeq "$(findstring dyn, $(GhcRTSWays))" "dyn"
BUILD_SHARED=yes
else
BUILD_SHARED=no
endif

# In a bindist, we don't want to know whether /this/ machine has gmp,
# but whether the machine the bindist was built on had gmp.
ifeq "$(BINDIST)" "YES"
ifeq "$(wildcard libraries/ghc-bignum/gmp/libgmp.a)" ""
HaveLibGmp = YES
HaveFrameworkGMP = YES
else
HaveLibGmp = NO
HaveFrameworkGMP = NO
endif
endif

UseIntreeGmp = NO
ifneq "$(HaveLibGmp)" "YES"
ifneq "$(HaveFrameworkGMP)" "YES"
UseIntreeGmp = YES
endif
endif

# gmp_wrappers.c includes "ghc-gmp.h"
libraries/ghc-bignum/cbits/gmp_wrappers.c: libraries/ghc-bignum/include/ghc-gmp.h

ifeq "$(UseIntreeGmp)" "YES"
# Copy header from in-tree build (gmp.h => ghc-gmp.h)
libraries/ghc-bignum/include/ghc-gmp.h: libraries/ghc-bignum/gmp/gmp.h
	$(CP) $< $@

# Link in-tree GMP objects
libraries/ghc-bignum_dist-install_EXTRA_OBJS += libraries/ghc-bignum/gmp/objs/*.o

else

# Copy header from source tree
libraries/ghc-bignum/include/ghc-gmp.h: libraries/ghc-bignum/gmp/ghc-gmp.h
	$(CP) $< $@

endif

libraries/ghc-bignum_dist-install_EXTRA_CC_OPTS += $(gmp_CC_OPTS)

ifneq "$(CLEANING)" "YES"
# When running `make clean` before `./configure`, CC_STAGE1 is undefined.
CLANG = $(findstring clang, $(shell $(CC_STAGE1) --version))

ifeq "$(CLANG)" "clang"
CCX = $(CLANG)
else
CCX = $(CC_STAGE1)
endif

libraries/ghc-bignum/gmp/libgmp.a libraries/ghc-bignum/gmp/gmp.h:
	$(RM) -rf libraries/ghc-bignum/gmp/$(GMP_DIR) libraries/ghc-bignum/gmp/gmpbuild libraries/ghc-bignum/gmp/objs
	cat $(GMP_TARBALL) | $(BZIP2_CMD) -d | { cd libraries/ghc-bignum/gmp && $(TAR_CMD) -xf - ; }
	mv libraries/ghc-bignum/gmp/$(GMP_DIR) libraries/ghc-bignum/gmp/gmpbuild
	cd libraries/ghc-bignum/gmp && $(PATCH_CMD) -p0 < gmpsrc.patch
	chmod +x libraries/ghc-bignum/gmp/ln

	# Note: We must pass `TARGETPLATFORM` to the `--host` argument of GMP's
	#       `./configure`, not `HOSTPLATFORM`: the 'host' on which GMP will
	#       run is the 'target' platform of the compiler we're building.
	# Note2: we pass --with-readline=no, to prevent getting an indirect
	#        dependency on ncurses through gmp.  readline is only relevant
	#        for gmp test programs. (See gmp's configure)
	cd libraries/ghc-bignum/gmp/gmpbuild; \
	    CC=$(CCX) CXX=$(CCX) NM=$(NM) AR=$(AR_STAGE1) ./configure \
	          --enable-shared=no --with-pic=yes --with-readline=no \
	          --host=$(TARGETPLATFORM) --build=$(BUILDPLATFORM)
	$(MAKE) -C libraries/ghc-bignum/gmp/gmpbuild MAKEFLAGS=
	$(CP) libraries/ghc-bignum/gmp/gmpbuild/gmp.h libraries/ghc-bignum/gmp/
	$(CP) libraries/ghc-bignum/gmp/gmpbuild/.libs/libgmp.a libraries/ghc-bignum/gmp/
	$(MKDIRHIER) libraries/ghc-bignum/gmp/objs
	cd libraries/ghc-bignum/gmp/objs && $(AR_STAGE1) x ../libgmp.a
	$(RANLIB_CMD) libraries/ghc-bignum/gmp/libgmp.a

endif # CLEANING
