
TOP=..

include $(TOP)/mk/boilerplate.mk

# -----------------------------------------------------------------------------
# This Makefile is copied from the one we use for GMP in ../gmp.
#
# We use libffi's own configuration stuff.

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

LIBFFI_TARBALL := $(firstword $(wildcard libffi*.tar.gz))
LIBFFI_DIR := $(subst .tar.gz,,$(LIBFFI_TARBALL))

ifeq "$(findstring dyn, $(GhcRTSWays))" "dyn"
BUILD_SHARED=yes
else
BUILD_SHARED=no
endif

boot :: stamp.ffi.static
BINDIST_STAMPS = stamp.ffi.static
INSTALL_HEADERS += ffi.h
INSTALL_LIBS += libffi.a

ifeq "$(BUILD_SHARED)" "yes"
boot :: stamp.ffi.shared
BINDIST_STAMPS += stamp.ffi.shared
INSTALL_LIBS += libffi.dll.a
INSTALL_PROGS += libffi-3.dll
endif

install all :: $(INSTALL_HEADERS) $(INSTALL_LIBS) $(INSTALL_PROGS)

# We have to fake a non-working ln for configure, so that the fallback
# option (cp -p) gets used instead.  Otherwise the libffi build system
# will use cygwin symbolic linkks which cannot be read by mingw gcc.
# The same trick is played by the GMP build in ../gmp.

stamp.ffi.static:
	$(RM) -rf $(LIBFFI_DIR) build
	$(TAR) -zxf $(LIBFFI_TARBALL)
	mv $(LIBFFI_DIR) build
	chmod +x ln
	(set -o igncr 2>/dev/null) && set -o igncr; export SHELLOPTS; \
	    PATH=`pwd`:$$PATH; \
	    export PATH; \
	    cd build && \
	    CC=$(WhatGccIsCalled) $(SHELL) configure \
	          --enable-shared=no --host=$(PLATFORM) --build=$(PLATFORM)
	touch $@

stamp.ffi.shared:
	$(RM) -rf $(LIBFFI_DIR) build-shared
	$(TAR) -zxf $(LIBFFI_TARBALL)
	mv $(LIBFFI_DIR) build-shared
	chmod +x ln
	(set -o igncr 2>/dev/null) && set -o igncr; export SHELLOPTS; \
	    PATH=`pwd`:$$PATH; \
	    export PATH; \
	    cd build-shared && \
	    CC=$(WhatGccIsCalled) $(SHELL) configure \
	          --enable-shared=yes --disable-static --host=$(PLATFORM) --build=$(PLATFORM)
	touch $@

ffi.h: stamp.ffi.static
	$(CP) build/include/ffi.h .

libffi.a: stamp.ffi.static
	$(MAKE) -C build MAKEFLAGS=
	$(CP) build/.libs/libffi.a .
	$(RANLIB) libffi.a

libffi-3.dll: stamp.ffi.shared
	$(MAKE) -C build-shared MAKEFLAGS=
	$(CP) build-shared/.libs/libffi-3.dll .

libffi.dll.a: libffi-3.dll
	$(CP) build-shared/.libs/libffi.dll.a .

clean distclean maintainer-clean ::
	$(RM) -f stamp.ffi.static stamp.ffi.shared ffi.h
	$(RM) -f libffi.a libffi-3.dll libffi.dll.a
	$(RM) -rf build
	$(RM) -rf build-shared

#-----------------------------------------------------------------------------
#
# binary-dist

include $(TOP)/mk/target.mk

binary-dist:
	$(INSTALL_DIR)                         $(BIN_DIST_DIR)/libffi
	$(INSTALL_DATA)    Makefile            $(BIN_DIST_DIR)/libffi/
ifneq "$(HaveLibFFI)" "YES"
	$(INSTALL_DATA)    $(BINDIST_STAMPS)   $(BIN_DIST_DIR)/libffi/
  ifneq "$(INSTALL_PROGS)" ""
	$(INSTALL_DATA)    $(INSTALL_PROGS)    $(BIN_DIST_DIR)/libffi/
  endif
  ifneq "$(INSTALL_LIBS)" ""
	$(INSTALL_DATA)    $(INSTALL_LIBS)     $(BIN_DIST_DIR)/libffi/
  endif
  ifneq "$(INSTALL_HEADERS)" ""
	$(INSTALL_HEADER)  $(INSTALL_HEADERS)  $(BIN_DIST_DIR)/libffi/
  endif
endif

