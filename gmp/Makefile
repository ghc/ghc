
TOP=..

include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------
# Compile GMP only if we don't have it already
#
# We use GMP's own configuration stuff, because it's all rather hairy
# and not worth re-implementing in our Makefile framework.

ifneq "$(HaveLibGmp)" "YES"
ifneq "$(HaveFrameworkGMP)" "YES"

boot :: stamp.gmp

PLATFORM := $(shell echo $(HOSTPLATFORM) | sed 's/i[567]86/i486/g')

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

GMP_TARBALL := $(firstword $(wildcard gmp*.tar.gz))
GMP_DIR := $(subst .tar.gz,,$(GMP_TARBALL))
BMP_BUILD_DIR := build

stamp.gmp:
	$(RM) -rf $(GMP_DIR) gmpbuild
	$(TAR) -zxf $(GMP_TARBALL)
	mv $(GMP_DIR) gmpbuild
	chmod +x ln
	set -o igncr; export SHELLOPTS; \
	    export PATH=`pwd`:$$PATH; \
	    cd gmpbuild && \
	    CC=$(WhatGccIsCalled) $(SHELL) configure \
	          --enable-shared=no --host=$(PLATFORM) --build=$(PLATFORM)
	touch $@

all :: gmpbuild/libgmp.a

ifeq "$(DLLized)" "YES"
all :: $(DLL_PEN)/gmp.dll

$(DLL_PEN)/gmp.dll:
	$(MAKE) -C gmpbuild gmp.dll
	$(CP) gmpbuild/gmp.dll $(DLL_PEN)
endif

install :: gmpbuild/libgmp.a

INSTALL_LIBS += gmpbuild/libgmp.a

gmpbuild/libgmp.a ::
	$(MAKE) -C gmpbuild MAKEFLAGS=
	$(CP) gmpbuild/.libs/libgmp.a gmpbuild
	$(RANLIB) gmpbuild/libgmp.a
endif
endif

clean distclean maintainer-clean ::
	$(RM) -f stamp.gmp
	$(RM) -rf gmpbuild
	$(RM) -rf gmpbuild

#-----------------------------------------------------------------------------
#
# Files to install
#
# Just libHSrts is installed uniformly across ways
#
ifeq "$(DLLized)" "YES"
INSTALL_PROGS += gmpbuild/gmp.dll
INSTALL_LIBS += gmpbuild/libgmp_imp.a
endif

#-----------------------------------------------------------------------------
#
# binary-dist

include $(TOP)/mk/target.mk

binary-dist:
	@:
ifneq "$(HaveLibGmp)" "YES"
ifneq "$(HaveFrameworkGMP)" "YES"
	$(INSTALL_DIR)                         $(BIN_DIST_DIR)/gmp
	$(INSTALL_DIR)                         $(BIN_DIST_DIR)/gmp/gmpbuild
	touch $(BIN_DIST_DIR)/gmp/$(GMP_TARBALL)
	$(INSTALL_DATA)    Makefile            $(BIN_DIST_DIR)/gmp/
ifneq "$(INSTALL_PROGS)" ""
	$(INSTALL_DATA)    $(INSTALL_PROGS)    $(BIN_DIST_DIR)/gmp/gmpbuild/
endif
ifneq "$(INSTALL_LIBS)" ""
	$(INSTALL_DATA)    $(INSTALL_LIBS)     $(BIN_DIST_DIR)/gmp/gmpbuild/
endif

endif
endif

