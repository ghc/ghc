
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

BINDIST_STAMPS = stamp.ffi
INSTALL_HEADERS += ffi.h
STATIC_LIB = libffi.a
INSTALL_LIBS += $(STATIC_LIB)

# We have to add the GHC version to the name of our dynamic libs, because
# they will be residing in the system location along with dynamic libs from
# other GHC installations.
ifeq "$(Windows)" "YES"
DYNAMIC_PROG = libffi.dll.a
DYNAMIC_LIBS = libffi-3.dll
RENAME_LIBS  =
else
DYNAMIC_PROG =
DYNAMIC_LIBS = libffi.so libffi.so.5 libffi.so.5.0.5
endif

ifeq "$(BuildSharedLibs)" "YES"
EnableShared=yes
else
EnableShared=no
endif

ifeq "$(BuildSharedLibs)" "YES"
INSTALL_LIBS  += $(DYNAMIC_LIBS)
INSTALL_PROGS += $(DYNAMIC_PROGS)
endif

install all :: $(INSTALL_HEADERS) $(INSTALL_LIBS) $(INSTALL_PROGS)

# We have to fake a non-working ln for configure, so that the fallback
# option (cp -p) gets used instead.  Otherwise the libffi build system
# will use cygwin symbolic linkks which cannot be read by mingw gcc.
# The same trick is played by the GMP build in ../gmp.

stamp.ffi:
	$(RM) -rf $(LIBFFI_DIR) build
	$(TAR) -zxf $(LIBFFI_TARBALL)
	mv $(LIBFFI_DIR) build
	chmod +x ln
	(set -o igncr 2>/dev/null) && set -o igncr; export SHELLOPTS; \
	    PATH=`pwd`:$$PATH; \
	    export PATH; \
	    cd build && \
	    CC=$(WhatGccIsCalled) $(SHELL) configure \
		  --enable-static=yes \
	          --enable-shared=$(EnableShared) \
		  --host=$(PLATFORM) --build=$(PLATFORM)
	touch $@

ffi.h: stamp.ffi
	$(CP) build/include/ffi.h .

$(STATIC_LIB) $(DYNAMIC_LIBS) $(DYNAMIC_PROG) : stamp.ffi
	$(MAKE) -C build MAKEFLAGS=
	(cd build; ./libtool --mode=install cp libffi.la $(FPTOOLS_TOP_ABS)/libffi)

clean distclean maintainer-clean ::
	$(RM) -f stamp.ffi ffi.h
	$(RM) -f libffi.a libffi.la $(DYNAMIC_PROG) $(DYNAMIC_LIBS) $(ORIG_DYNAMIC_LIBS)
	$(RM) -rf build

#-----------------------------------------------------------------------------
#
# binary-dist

include $(TOP)/mk/target.mk

BINDIST_EXTRAS += $(BINDIST_STAMPS)
BINDIST_EXTRAS += $(INSTALL_PROGS)
BINDIST_EXTRAS += $(INSTALL_LIBS)
BINDIST_EXTRAS += $(INSTALL_HEADERS)
include $(TOP)/mk/bindist.mk

