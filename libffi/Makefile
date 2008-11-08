
TOP=..
DONT_WANT_STD_GHCI_LIB_RULE=YES
DONT_WANT_STD_LIBRARY=YES

include $(TOP)/mk/boilerplate.mk
# Override haddock generation for this package
HADDOCK_DOCS=NO

# We package libffi as Haskell package for two reasons: 

# 1) GHC uses different names for shared and static libs, so it can
#    choose the lib variant to link with on its own. With regular
#    libtool styled shared lib names, the linker would interfer and
#    link against the shared lib variant even when GHC runs in -static
#    mode.
# 2) The first issue isn't a problem when a shared lib of libffi would
#    be installed in system locations, but we do not assume that. So,
#    when running in -dynamic mode, we must either install libffi to
#    system locations ourselves, or we must add its location to
#    respective environment variable, (DY)LD_LIBRARY_PATH etc...before
#    we call dynamically linked binaries. Especially, the latter is
#    necessary as GHC calls binary it produced before its installation
#    phase. However, both mechanism, installing to system locations or
#    modifying (DY)LD_LIBRARY_PATH, are already in place for Haskell
#    packages so with packaging libffi as Haskell package we reuse
#    them naturally.

PACKAGE=ffi
# -----------------------------------------------------------------------------
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

BINDIST_STAMPS = stamp.ffi.build stamp.ffi.configure

ifeq "$(BuildSharedLibs)" "YES"
STAMP_BUILD = stamp.ffi.build-shared
STAMP_CONFIGURE = stamp.ffi.configure-shared
else
STAMP_BUILD = stamp.ffi.build
STAMP_CONFIGURE = stamp.ffi.configure
endif

INSTALL_HEADERS += ffi.h
STATIC_LIB = libffi.a
INSTALL_LIBS += libHSffi.a HSffi.o

# We have to add the GHC version to the name of our dynamic libs, because
# they will be residing in the system location along with dynamic libs from
# other GHC installations.

HS_DYN_LIB_NAME=libHSffi-ghc$(ProjectVersion)$(soext)

ifeq "$(Windows)" "YES"
DYNAMIC_PROG = $(HS_DYN_LIB_NAME).a
DYNAMIC_LIBS = $(HS_DYN_LIB_NAME)
else
DYNAMIC_PROG =
DYNAMIC_LIBS = libffi.so libffi.so.5 libffi.so.5.0.7
endif

ifeq "$(BuildSharedLibs)" "YES"
EnableShared=yes
else
EnableShared=no
endif

ifeq "$(BuildSharedLibs)" "YES"
INSTALL_LIBS  += $(HS_DYN_LIB_NAME)
ifeq "$(Windows)" "YES"
INSTALL_PROGS += $(HS_DYN_LIB_NAME).a
endif
endif

install all :: $(INSTALL_HEADERS) $(INSTALL_LIBS) $(INSTALL_PROGS)

# We have to fake a non-working ln for configure, so that the fallback
# option (cp -p) gets used instead.  Otherwise the libffi build system
# will use cygwin symbolic linkks which cannot be read by mingw gcc.
# The same trick is played by the GMP build in ../gmp.

$(STAMP_CONFIGURE):
	$(RM) -rf $(LIBFFI_DIR) build
	$(TAR) -zxf $(LIBFFI_TARBALL)
	mv $(LIBFFI_DIR) build
	chmod +x ln
	patch -p0 < libffi-dllize-3.0.6.patch

	# This patch is just the resulting delta from running automake, autoreconf, libtoolize --force --copy
	patch -p0 < libffi-autotools-update.patch

	(set -o igncr 2>/dev/null) && set -o igncr; export SHELLOPTS; \
	    PATH=`pwd`:$$PATH; \
	    export PATH; \
	    cd build && \
	    CC=$(WhatGccIsCalled) $(SHELL) configure \
		  --enable-static=yes \
	          --enable-shared=$(EnableShared) \
		  --host=$(PLATFORM) --build=$(PLATFORM)

	# libffi.so needs to be built with the correct soname.
	# NOTE: this builds libffi_convience.so with the incorrect
	# soname, but we don't need that anyway!
	$(CP) build/libtool build/libtool.orig
	sed -e s/soname_spec=.*/soname_spec="$(HS_DYN_LIB_NAME)"/ build/libtool.orig > build/libtool

	# We don't want libtool's cygwin hacks
	$(CP) build/libtool build/libtool.orig
	sed -e s/dlname=\'\$$tdlname\'/dlname=\'\$$dlname\'/ build/libtool.orig > build/libtool

	touch $@

ffi.h: $(STAMP_CONFIGURE)
	$(CP) build/include/ffi.h .

$(STAMP_BUILD): $(STAMP_CONFIGURE)
	$(MAKE) -C build MAKEFLAGS=
	(cd build; ./libtool --mode=install cp libffi.la $(FPTOOLS_TOP_ABS)/libffi)
	$(CP) $(STATIC_LIB) libHSffi.a
	$(CP) $(STATIC_LIB) libHSffi_p.a
	touch $@

libHSffi.a libHSffi_p.a: $(STAMP_BUILD)

all :: libHSffi.a libHSffi_p.a

# The GHCi import lib isn't needed as compiler/ghci/Linker.lhs + rts/Linker.c
# link the interpreted references to FFI to the compiled FFI.
# Instead of adding libffi to the list preloaded packages (see
# compiler/ghci/Linker.lhs:emptyPLS) we generate an empty HSffi.o

HSffi.o: libHSffi.a
	touch empty.c
	$(CC) -c empty.c -o HSffi.o

all :: HSffi.o

ifeq "$(BuildSharedLibs)" "YES"
ifeq "$(Windows)" "YES"
# Windows libtool creates <soname>.dll, and as we already patched that
# there is no need to copy from libffi.dll to libHSffi...dll.
# However, the renaming is still required for the import library
# libffi.dll.a.
$(HS_DYN_LIB_NAME).a: $(STAMP_BUILD)
	$(CP) libffi.dll.a $(HS_DYN_LIB_NAME).a
all :: $(HS_DYN_LIB_NAME).a

else
# Rename libffi.so to libHSffi...so
$(HS_DYN_LIB_NAME): $(DYNAMIC_LIBS)
	$(CP) $(word 1,$(DYNAMIC_LIBS)) $(HS_DYN_LIB_NAME)

all :: $(HS_DYN_LIB_NAME)
endif
endif

clean distclean maintainer-clean ::
	$(RM) -f stamp.ffi.* ffi.h empty.c
	$(RM) -f libffi.a libffi.la $(DYNAMIC_PROG) $(DYNAMIC_LIBS) $(HS_DYN_LIB_NAME) $(HS_DYN_LIB_NAME).a
	$(RM) -rf build

#-----------------------------------------------------------------------------
#
# binary-dist

include $(TOP)/mk/target.mk

BINDIST_EXTRAS += package.conf.in
BINDIST_EXTRAS += $(BINDIST_STAMPS)
BINDIST_EXTRAS += $(INSTALL_PROGS)
BINDIST_EXTRAS += $(INSTALL_LIBS)
BINDIST_EXTRAS += $(INSTALL_HEADERS)
include $(TOP)/mk/bindist.mk

