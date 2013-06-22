# -----------------------------------------------------------------------------
#
# (c) 2009 The University of Glasgow
#
# This file is part of the GHC build system.
#
# To understand how the build system works and how to modify it, see
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Architecture
#      http://hackage.haskell.org/trac/ghc/wiki/Building/Modifying
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

ifeq "$(findstring clean,$(MAKECMDGOALS))" ""
include libraries/integer-gmp/gmp/config.mk
endif

libraries/integer-gmp_dist-install_EXTRA_CC_OPTS += -Ilibraries/integer-gmp/mkGmpDerivedConstants/dist
libraries/integer-gmp_dist-install_EXTRA_HC_OPTS += -Ilibraries/integer-gmp/mkGmpDerivedConstants/dist

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

$(libraries/integer-gmp_dist-install_depfile_c_asm): $$(GmpDerivedConstants_HEADER)

ifneq "$(HaveLibGmp)" "YES"
ifneq "$(HaveFrameworkGMP)" "YES"
$(libraries/integer-gmp_dist-install_depfile_c_asm): libraries/integer-gmp/gmp/gmp.h

gmp_CC_OPTS += -Ilibraries/integer-gmp/gmp
gmp_CC_OPTS += -Ilibraries/integer-gmp/mkGmpDerivedConstants/dist

libraries/integer-gmp_dist-install_EXTRA_OBJS += libraries/integer-gmp/gmp/objs/*.o

#INSTALL_LIBS += libraries/integer-gmp/gmp/libgmp.a
#INSTALL_HEADERS += libraries/integer-gmp/gmp/gmp.h
#
#$(eval $(call all-target,gmp_dynamic,libraries/integer-gmp/gmp/libgmp.a))
#
#ifeq "$(BUILD_SHARED)" "yes"
#$(eval $(call all-target,gmp_dynamic,libraries/integer-gmp/gmp/libgmp.dll.a libraries/integer-gmp/gmp/libgmp-3.dll))
#endif

endif
endif

libraries/integer-gmp_dist-install_EXTRA_CC_OPTS += $(gmp_CC_OPTS)

# 2007-09-26
#     set -o igncr 
# is not a valid command on non-Cygwin-systems.
# Let it fail silently instead of aborting the build.
#
# 2007-07-05
# We do
#     set -o igncr; export SHELLOPTS
# here as otherwise checking the size of limbs
# makes the build fall over on Cygwin. See the thread
# http://www.cygwin.com/ml/cygwin/2006-12/msg00011.html
# for more details.

# 2007-07-05
# Passing
#     as_ln_s='cp -p'
# isn't sufficient to stop cygwin using symlinks the mingw gcc can't
# follow, as it isn't used consistently. Instead we put an ln.bat in
# path that always fails.

libraries/integer-gmp/gmp/libgmp.a libraries/integer-gmp/gmp/gmp.h:
	$(RM) -rf libraries/integer-gmp/gmp/$(GMP_DIR) libraries/integer-gmp/gmp/gmpbuild libraries/integer-gmp/gmp/objs
	cat $(GMP_TARBALL) | $(BZIP2_CMD) -d | { cd libraries/integer-gmp/gmp && $(TAR_CMD) -xf - ; }
	mv libraries/integer-gmp/gmp/$(GMP_DIR) libraries/integer-gmp/gmp/gmpbuild
	chmod +x libraries/integer-gmp/gmp/ln

	# Their cmd invocation only works on msys. On cygwin it starts
	# a cmd interactive shell. The replacement works in both environments.
	mv libraries/integer-gmp/gmp/gmpbuild/ltmain.sh libraries/integer-gmp/gmp/gmpbuild/ltmain.sh.orig
	sed 's#cmd //c echo "\$$1"#cmd /c "echo $$1"#' < libraries/integer-gmp/gmp/gmpbuild/ltmain.sh.orig > libraries/integer-gmp/gmp/gmpbuild/ltmain.sh

	cd libraries/integer-gmp/gmp; (set -o igncr 2>/dev/null) && set -o igncr; export SHELLOPTS; \
	    PATH=`pwd`:$$PATH; \
	    export PATH; \
	    cd gmpbuild && \
	    CC=$(CC_STAGE1) NM=$(NM) AR=$(AR_STAGE1) $(SHELL) ./configure \
	          --enable-shared=no \
	          --host=$(HOSTPLATFORM) --build=$(BUILDPLATFORM)
	$(MAKE) -C libraries/integer-gmp/gmp/gmpbuild MAKEFLAGS=
	$(CP) libraries/integer-gmp/gmp/gmpbuild/gmp.h libraries/integer-gmp/gmp/
	$(CP) libraries/integer-gmp/gmp/gmpbuild/.libs/libgmp.a libraries/integer-gmp/gmp/
	$(MKDIRHIER) libraries/integer-gmp/gmp/objs
	cd libraries/integer-gmp/gmp/objs && $(AR_STAGE1) x ../libgmp.a
	$(RANLIB) libraries/integer-gmp/gmp/libgmp.a

# XXX TODO:
#stamp.gmp.shared:
#	$(RM) -rf $(GMP_DIR) gmpbuild-shared
#	$(TAR_CMD) -zxf $(GMP_TARBALL)
#	mv $(GMP_DIR) gmpbuild-shared
#	chmod +x ln
#	(set -o igncr 2>/dev/null) && set -o igncr; export SHELLOPTS; \
#	    PATH=`pwd`:$$PATH; \
#	    export PATH; \
#	    cd gmpbuild-shared && \
#	    CC=$(CC_STAGE1) $(SHELL) ./configure \
#	          --enable-shared=yes --disable-static \
#	          --host=$(HOSTPLATFORM) --build=$(BUILDPLATFORM)
#	"$(TOUCH_CMD)" $@
#
#gmp.h: stamp.gmp.static
#	$(CP) gmpbuild/gmp.h .
#
#libgmp.a: stamp.gmp.static
#
#libgmp-3.dll: stamp.gmp.shared
#	$(MAKE) -C gmpbuild-shared MAKEFLAGS=
#	$(CP) gmpbuild-shared/.libs/libgmp-3.dll .
#
#libgmp.dll.a: libgmp-3.dll
#	$(CP) gmpbuild-shared/.libs/libgmp.dll.a .

## GMP takes a long time to build, but changes rarely.  Hence we don't
## bother cleaning it before validating, because that adds a
## significant overhead to validation.
#ifeq "$(Validating)" "NO"
#clean distclean maintainer-clean ::
#	$(RM) -f stamp.gmp.static stamp.gmp.shared
#	$(RM) -rf gmpbuild
#	$(RM) -rf gmpbuild-shared
#endif

endif

