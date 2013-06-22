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


libffi_STAMP_STATIC_CONFIGURE = libffi/stamp.ffi.static.configure
libffi_STAMP_STATIC_BUILD     = libffi/stamp.ffi.static.build
libffi_STAMP_STATIC_INSTALL   = libffi/stamp.ffi.static.install

libffi_STAMP_STATIC_SHARED_CONFIGURE = libffi/stamp.ffi.static-shared.configure
libffi_STAMP_STATIC_SHARED_BUILD     = libffi/stamp.ffi.static-shared.build
libffi_STAMP_STATIC_SHARED_INSTALL   = libffi/stamp.ffi.static-shared.install

ifeq "$(BuildSharedLibs)" "YES"
libffi_STAMP_CONFIGURE = $(libffi_STAMP_STATIC_SHARED_CONFIGURE)
libffi_STAMP_BUILD     = $(libffi_STAMP_STATIC_SHARED_BUILD)
libffi_STAMP_INSTALL   = $(libffi_STAMP_STATIC_SHARED_INSTALL)
libffi_EnableShared    = yes
else
libffi_STAMP_CONFIGURE = $(libffi_STAMP_STATIC_CONFIGURE)
libffi_STAMP_BUILD     = $(libffi_STAMP_STATIC_BUILD)
libffi_STAMP_INSTALL   = $(libffi_STAMP_STATIC_INSTALL)
libffi_EnableShared    = no
endif

libffi_STATIC_LIB  = libffi/build/inst/lib/libffi.a
libffi_HEADERS     = rts/dist/build/ffi.h \
                     rts/dist/build/ffitarget.h

ifeq "$(HostOS_CPP)" "mingw32"
LIBFFI_NAME = ffi-6
else
LIBFFI_NAME = ffi
endif
LIBFFI_DLL = lib$(LIBFFI_NAME).dll

ifeq "$(OSTYPE)" "cygwin"
LIBFFI_PATH_MANGLE = PATH=$$(cygpath "$(TOP)")/libffi:$$PATH; export PATH;
endif

ifneq "$(BINDIST)" "YES"
$(libffi_STAMP_CONFIGURE): $(TOUCH_DEP)
	$(call removeFiles,$(libffi_STAMP_STATIC_CONFIGURE))
	$(call removeFiles,$(libffi_STAMP_STATIC_BUILD))
	$(call removeFiles,$(libffi_STAMP_STATIC_INSTALL))
	$(call removeFiles,$(libffi_STAMP_STATIC_SHARED_CONFIGURE))
	$(call removeFiles,$(libffi_STAMP_STATIC_SHARED_BUILD))
	$(call removeFiles,$(libffi_STAMP_STATIC_SHARED_INSTALL))
	$(call removeTrees,$(LIBFFI_DIR) libffi/build)
	cat ghc-tarballs/libffi/libffi*.tar.gz | $(GZIP_CMD) -d | { cd libffi && $(TAR_CMD) -xf - ; }
	mv libffi/libffi-* libffi/build

# We have to fake a non-working ln for configure, so that the fallback
# option (cp -p) gets used instead.  Otherwise the libffi build system
# will use cygwin symbolic links which cannot be read by mingw gcc.
	chmod +x libffi/ln

	# We need to use -MMD rather than -MD, as otherwise we get paths
	# like c:/... in the dependency files on Windows, and the extra
	# colons break make
	mv libffi/build/Makefile.in libffi/build/Makefile.in.orig
	sed "s/-MD/-MMD/" < libffi/build/Makefile.in.orig > libffi/build/Makefile.in

	# Their cmd invocation only works on msys. On cygwin it starts
	# a cmd interactive shell. The replacement works in both environments.
	mv libffi/build/ltmain.sh libffi/build/ltmain.sh.orig
	sed 's#cmd //c echo "\$$1"#cmd /c "echo $$1"#' < libffi/build/ltmain.sh.orig > libffi/build/ltmain.sh

# * Because -Werror may be in SRC_CC_OPTS/SRC_LD_OPTS, we need to turn
#   warnings off or the compilation of libffi might fail due to warnings;
#   hence the -w flags.
# * We specify --libdir, as we need to know the path to libffi.a, but on
#   some platforms it defaults to .../lib64/ rather than .../lib/.
	cd libffi && \
	    $(LIBFFI_PATH_MANGLE) \
	    cd build && \
	    CC=$(CC_STAGE1) \
	    LD=$(LD) \
	    AR=$(AR_STAGE1) \
	    NM=$(NM) \
        CFLAGS="$(SRC_CC_OPTS) $(CONF_CC_OPTS_STAGE1) -w" \
        LDFLAGS="$(SRC_LD_OPTS) $(CONF_GCC_LINKER_OPTS_STAGE1) -w" \
        "$(SHELL)" ./configure \
	          --prefix=$(TOP)/libffi/build/inst \
	          --libdir=$(TOP)/libffi/build/inst/lib \
	          --enable-static=yes \
	          --enable-shared=$(libffi_EnableShared) \
	          --host=$(TargetPlatformFull)

	# wc on OS X has spaces in its output, which libffi's Makefile
	# doesn't expect, so we tweak it to sed them out
	mv libffi/build/Makefile libffi/build/Makefile.orig
	sed "s#wc -w#wc -w | sed 's/ //g'#" < libffi/build/Makefile.orig > libffi/build/Makefile

	"$(TOUCH_CMD)" $@

$(libffi_STAMP_BUILD): $(libffi_STAMP_CONFIGURE) $(TOUCH_DEP)
	$(MAKE) -C libffi/build MAKEFLAGS=
	"$(TOUCH_CMD)" $@

$(libffi_STAMP_INSTALL): $(libffi_STAMP_BUILD) $(TOUCH_DEP)
	$(MAKE) -C libffi/build MAKEFLAGS= install
	"$(TOUCH_CMD)" $@

$(libffi_STATIC_LIB): $(libffi_STAMP_INSTALL)
	@test -f $@ || { echo "$< exists, but $@ does not."; echo "Suggest removing $<."; exit 1; }

$(libffi_HEADERS): $(libffi_STAMP_INSTALL) | $$(dir $$@)/.
	cp -f libffi/build/inst/lib/libffi-*/include/$(notdir $@) $@

$(eval $(call clean-target,libffi,, \
    libffi/build $(wildcard libffi/stamp.ffi.*) libffi/dist-install))

endif

