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
  libraries/integer-gmp2/include/ghc-gmp.h \
  libraries/integer-gmp2/gmp/config.mk \
  libraries/integer-gmp2/gmp/libgmp.a \
  libraries/integer-gmp2/gmp/gmp.h \
  libraries/integer-gmp2/gmp/gmpbuild \
  libraries/integer-gmp2/gmp/$(GMP_DIR)))

clean : clean_gmp
.PHONY: clean_gmp
clean_gmp:
	$(call removeTrees,libraries/integer-gmp2/gmp/objs)
	$(call removeTrees,libraries/integer-gmp2/gmp/gmpbuild)
endif

ifeq "$(Windows_Host)" "YES"
# Apparently building on Windows fails when there is a system gmp
# available, so we never try to use the system gmp on Windows
libraries/integer-gmp2_CONFIGURE_OPTS += --configure-option=--with-intree-gmp
endif

ifeq "$(GMP_PREFER_FRAMEWORK)" "YES"
libraries/integer-gmp2_CONFIGURE_OPTS += --with-gmp-framework-preferred
endif

ifeq "$(phase)" "final"

ifeq "$(findstring clean,$(MAKECMDGOALS))" ""
include libraries/integer-gmp2/gmp/config.mk
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
ifeq "$(wildcard libraries/integer-gmp2/gmp/libgmp.a)" ""
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
$(libraries/integer-gmp2_dist-install_depfile_c_asm): libraries/integer-gmp2/gmp/gmp.h libraries/integer-gmp2/include/ghc-gmp.h

libraries/integer-gmp2/include/ghc-gmp.h: libraries/integer-gmp2/gmp/gmp.h
	$(CP) $< $@

gmp_CC_OPTS += -Ilibraries/integer-gmp2/gmp

libraries/integer-gmp2_dist-install_EXTRA_OBJS += libraries/integer-gmp2/gmp/objs/*.o
else
$(libraries/integer-gmp2_dist-install_depfile_c_asm): libraries/integer-gmp2/include/ghc-gmp.h

libraries/integer-gmp2/include/ghc-gmp.h: libraries/integer-gmp2/gmp/ghc-gmp.h
	$(CP) $< $@
endif

libraries/integer-gmp2_dist-install_EXTRA_CC_OPTS += $(gmp_CC_OPTS)

CLANG = $(findstring clang, $(shell $(CC_STAGE1) --version))

ifeq "$(CLANG)" "clang"
CCX = $(CLANG)
else
CCX = $(CC_STAGE1)
endif

libraries/integer-gmp2/gmp/libgmp.a libraries/integer-gmp2/gmp/gmp.h:
	$(RM) -rf libraries/integer-gmp2/gmp/$(GMP_DIR) libraries/integer-gmp2/gmp/gmpbuild libraries/integer-gmp2/gmp/objs
	cat $(GMP_TARBALL) | $(BZIP2_CMD) -d | { cd libraries/integer-gmp2/gmp && $(TAR_CMD) -xf - ; }
	mv libraries/integer-gmp2/gmp/$(GMP_DIR) libraries/integer-gmp2/gmp/gmpbuild
	cd libraries/integer-gmp2/gmp && $(PATCH_CMD) -p0 < gmpsrc.patch
	cat libraries/integer-gmp/gmp/tarball/gmp-5.0.4.patch | { cd libraries/integer-gmp2/gmp/gmpbuild && $(PATCH_CMD) -p1 ; }
	chmod +x libraries/integer-gmp2/gmp/ln

	# Their cmd invocation only works on msys. On cygwin it starts
	# a cmd interactive shell. The replacement works in both environments.
	mv libraries/integer-gmp2/gmp/gmpbuild/ltmain.sh libraries/integer-gmp2/gmp/gmpbuild/ltmain.sh.orig
	sed 's#cmd //c echo "\$$1"#cmd /c "echo $$1"#' < libraries/integer-gmp2/gmp/gmpbuild/ltmain.sh.orig > libraries/integer-gmp2/gmp/gmpbuild/ltmain.sh

	cd libraries/integer-gmp2/gmp; (set -o igncr 2>/dev/null) && set -o igncr; export SHELLOPTS; \
	    PATH=`pwd`:$$PATH; \
	    export PATH; \
	    cd gmpbuild && \
	    CC=$(CCX) NM=$(NM) AR=$(AR_STAGE1) ./configure \
	          --enable-shared=no \
	          --host=$(HOSTPLATFORM) --build=$(BUILDPLATFORM)
	$(MAKE) -C libraries/integer-gmp2/gmp/gmpbuild MAKEFLAGS=
	$(CP) libraries/integer-gmp2/gmp/gmpbuild/gmp.h libraries/integer-gmp2/gmp/
	$(CP) libraries/integer-gmp2/gmp/gmpbuild/.libs/libgmp.a libraries/integer-gmp2/gmp/
	$(MKDIRHIER) libraries/integer-gmp2/gmp/objs
	cd libraries/integer-gmp2/gmp/objs && $(AR_STAGE1) x ../libgmp.a
	$(RANLIB_CMD) libraries/integer-gmp2/gmp/libgmp.a

endif
