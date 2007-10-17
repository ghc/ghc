
TOP=..

include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------
# Compile GMP only if we don't have it already
#
# We use GMP's own configuration stuff, because it's all rather hairy
# and not worth re-implementing in our Makefile framework.

ifneq "$(HaveLibGmp)" "YES"
ifneq "$(HaveFrameworkGMP)" "YES"

PLATFORM := $(shell echo $(HOSTPLATFORM) | sed 's/i[567]86/i486/g')

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

GMP_TARBALL := $(firstword $(wildcard gmp*.tar.gz))
GMP_DIR := $(subst .tar.gz,,$(GMP_TARBALL))
BMP_BUILD_DIR := build

ifeq "$(findstring dyn, $(GhcRTSWays))" "dyn"
BUILD_SHARED=yes
else
BUILD_SHARED=no
endif

boot :: stamp.gmp.static
all :: gmp.h libgmp.a
install :: gmp.h libgmp.a
INSTALL_HEADERS += gmp.h
INSTALL_LIBS += libgmp.a

ifeq "$(BUILD_SHARED)" "yes"
boot :: stamp.gmp.shared
all :: libgmp-3.dll libgmp.dll.a
install :: libgmp-3.dll libgmp.dll.a
INSTALL_LIBS += libgmp.dll.a
INSTALL_PROGS += libgmp-3.dll
endif

stamp.gmp.static:
	$(RM) -rf $(GMP_DIR) gmpbuild
	$(TAR) -zxf $(GMP_TARBALL)
	mv $(GMP_DIR) gmpbuild
	chmod +x ln
	(set -o igncr 2>/dev/null) && set -o igncr; export SHELLOPTS; \
	    export PATH=`pwd`:$$PATH; \
	    cd gmpbuild && \
	    CC=$(WhatGccIsCalled) $(SHELL) configure \
	          --enable-shared=no --host=$(PLATFORM) --build=$(PLATFORM)
	touch $@

stamp.gmp.shared:
	$(RM) -rf $(GMP_DIR) gmpbuild-shared
	$(TAR) -zxf $(GMP_TARBALL)
	mv $(GMP_DIR) gmpbuild-shared
	chmod +x ln
	(set -o igncr 2>/dev/null) && set -o igncr; export SHELLOPTS; \
	    export PATH=`pwd`:$$PATH; \
	    cd gmpbuild-shared && \
	    CC=$(WhatGccIsCalled) $(SHELL) configure \
	          --enable-shared=yes --disable-static --host=$(PLATFORM) --build=$(PLATFORM)
	touch $@

gmp.h: stamp.gmp.static
	$(CP) gmpbuild/gmp.h .

libgmp.a: stamp.gmp.static
	$(MAKE) -C gmpbuild MAKEFLAGS=
	$(CP) gmpbuild/.libs/libgmp.a .
	$(RANLIB) libgmp.a

libgmp-3.dll: stamp.gmp.shared
	$(MAKE) -C gmpbuild-shared MAKEFLAGS=
	$(CP) gmpbuild-shared/.libs/libgmp-3.dll .

libgmp.dll.a: libgmp-3.dll
	$(CP) gmpbuild-shared/.libs/libgmp.dll.a .
endif
endif

# GMP takes a long time to build, but changes rarely.  Hence we don't
# bother cleaning it before validating, because that adds a
# significant overhead to validation.
ifeq "$(Validating)" "NO"
clean distclean maintainer-clean ::
	$(RM) -f stamp.gmp.static stamp.gmp.shared
	$(RM) -rf gmpbuild
	$(RM) -rf gmpbuild-shared
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
	touch $(BIN_DIST_DIR)/gmp/$(GMP_TARBALL)
	$(INSTALL_DATA)    Makefile            $(BIN_DIST_DIR)/gmp/
ifneq "$(INSTALL_PROGS)" ""
	$(INSTALL_DATA)    $(INSTALL_PROGS)    $(BIN_DIST_DIR)/gmp/
endif
ifneq "$(INSTALL_LIBS)" ""
	$(INSTALL_DATA)    $(INSTALL_LIBS)     $(BIN_DIST_DIR)/gmp/
endif
ifneq "$(INSTALL_HEADERS)" ""
	$(INSTALL_HEADER)  $(INSTALL_HEADERS)  $(BIN_DIST_DIR)/gmp/
endif

endif
endif

