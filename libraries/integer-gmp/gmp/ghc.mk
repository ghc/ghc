# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://ghc.haskell.org/trac/ghc/wiki/Building/Modifying
#
# -----------------------------------------------------------------------------

# We use a tarball like gmp-4.2.4-nodoc.tar.bz2, which is
# gmp-4.2.4.tar.bz2 repacked without the doc/ directory contents.
# That's because the doc/ directory contents are under the GFDL,
# which causes problems for Debian.

GMP_TARBALL := $(wildcard libraries/integer-gmp/gmp/tarball/gmp*.tar.bz2)
GMP_DIR := $(patsubst libraries/integer-gmp/gmp/tarball/%-nodoc-patched.tar.bz2,%,$(GMP_TARBALL))

ifneq "$(NO_CLEAN_GMP)" "YES"
$(eval $(call clean-target,gmp,,\
  libraries/integer-gmp/include/ghc-gmp.h \
  libraries/integer-gmp/gmp/config.mk \
  libraries/integer-gmp/gmp/libgmp.a \
  libraries/integer-gmp/gmp/gmp.h \
  libraries/integer-gmp/gmp/gmpbuild \
  libraries/integer-gmp/gmp/$(GMP_DIR)))

clean : clean_gmp
.PHONY: clean_gmp
clean_gmp:
	$(call removeTrees,libraries/integer-gmp/gmp/objs)
	$(call removeTrees,libraries/integer-gmp/gmp/gmpbuild)
endif

ifeq "$(Windows_Host)" "YES"
# Apparently building on Windows fails when there is a system gmp
# available, so we never try to use the system gmp on Windows
libraries/integer-gmp_CONFIGURE_OPTS += --configure-option=--with-intree-gmp
endif

ifeq "$(GMP_PREFER_FRAMEWORK)" "YES"
libraries/integer-gmp_CONFIGURE_OPTS += --with-gmp-framework-preferred
endif

ifeq "$(phase)" "final"

ifneq "$(CLEANING)" "YES"
# Hack. The file gmp/config.mk doesn't exist yet after running ./configure in
# the toplevel (ghc) directory. To let some toplevel make commands such as
# sdist go through, right after ./configure, don't consider this an error.
-include libraries/integer-gmp/gmp/config.mk
endif

gmp_CC_OPTS += $(addprefix -I,$(GMP_INCLUDE_DIRS))
gmp_CC_OPTS += $(addprefix -L,$(GMP_LIB_DIRS))

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
ifeq "$(wildcard libraries/integer-gmp/gmp/libgmp.a)" ""
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

ifeq "$(UseIntreeGmp)" "YES"
$(libraries/integer-gmp_dist-install_depfile_c_asm): libraries/integer-gmp/gmp/gmp.h libraries/integer-gmp/include/ghc-gmp.h

libraries/integer-gmp/include/ghc-gmp.h: libraries/integer-gmp/gmp/gmp.h
	$(CP) $< $@

gmp_CC_OPTS += -Ilibraries/integer-gmp/gmp

libraries/integer-gmp_dist-install_EXTRA_OBJS += libraries/integer-gmp/gmp/objs/*.o
else
$(libraries/integer-gmp_dist-install_depfile_c_asm): libraries/integer-gmp/include/ghc-gmp.h

libraries/integer-gmp/include/ghc-gmp.h: libraries/integer-gmp/gmp/ghc-gmp.h
	$(CP) $< $@
endif

libraries/integer-gmp_dist-install_EXTRA_CC_OPTS += $(gmp_CC_OPTS)

ifneq "$(CLEANING)" "YES"
# When running `make clean` before `./configure`, CC_STAGE1 is undefined.
CLANG = $(findstring clang, $(shell $(CC_STAGE1) --version))

ifeq "$(CLANG)" "clang"
CCX = $(CLANG)
else
CCX = $(CC_STAGE1)
endif

libraries/integer-gmp/gmp/libgmp.a libraries/integer-gmp/gmp/gmp.h:
	$(RM) -rf libraries/integer-gmp/gmp/$(GMP_DIR) libraries/integer-gmp/gmp/gmpbuild libraries/integer-gmp/gmp/objs
	cat $(GMP_TARBALL) | $(BZIP2_CMD) -d | { cd libraries/integer-gmp/gmp && $(TAR_CMD) -xf - ; }
	mv libraries/integer-gmp/gmp/$(GMP_DIR) libraries/integer-gmp/gmp/gmpbuild
	cd libraries/integer-gmp/gmp && $(PATCH_CMD) -p0 < gmpsrc.patch
	cat libraries/integer-gmp/gmp/tarball/gmp-5.0.4.patch | { cd libraries/integer-gmp/gmp/gmpbuild && $(PATCH_CMD) -p1 ; }
	chmod +x libraries/integer-gmp/gmp/ln

	# Note: We must pass `TARGETPLATFORM` to the `--host` argument of GMP's
	#       `./configure`, not `HOSTPLATFORM`: the 'host' on which GMP will
	#       run is the 'target' platform of the compiler we're building.
	cd libraries/integer-gmp/gmp; (set -o igncr 2>/dev/null) && set -o igncr; export SHELLOPTS; \
	    PATH=`pwd`:$$PATH; \
	    export PATH; \
	    cd gmpbuild && \
	    CC=$(CCX) NM=$(NM) AR=$(AR_STAGE1) ./configure \
	          --enable-shared=no \
	          --host=$(TARGETPLATFORM) --build=$(BUILDPLATFORM)
	$(MAKE) -C libraries/integer-gmp/gmp/gmpbuild MAKEFLAGS=
	$(CP) libraries/integer-gmp/gmp/gmpbuild/gmp.h libraries/integer-gmp/gmp/
	$(CP) libraries/integer-gmp/gmp/gmpbuild/.libs/libgmp.a libraries/integer-gmp/gmp/
	$(MKDIRHIER) libraries/integer-gmp/gmp/objs
	cd libraries/integer-gmp/gmp/objs && $(AR_STAGE1) x ../libgmp.a
	$(RANLIB_CMD) libraries/integer-gmp/gmp/libgmp.a

endif # CLEANING
endif # phase
