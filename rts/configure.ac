# Configure script template for the Run-time System of GHC
#
# Process with 'autoreconf' to get a working configure script.
#
# For the generated configure script, do "./configure --help" to
# see what flags are available. (Better yet, read the documentation!)
#

AC_INIT([GHC run-time system], [1.0.2], [libraries@haskell.org], [rts])

AC_CONFIG_MACRO_DIRS([../m4])

# Safety check: Ensure that we are in the correct source directory.
AC_CONFIG_SRCDIR([include/rts/Constants.h])

dnl * We require autoconf version 2.69 due to
dnl   https://bugs.ruby-lang.org/issues/8179. Also see #14910.
dnl * We need 2.50 due to the use of AC_SYS_LARGEFILE and AC_MSG_NOTICE.
dnl * We need 2.52 due to the use of AS_TR_CPP and AS_TR_SH.
dnl * Using autoconf 2.59 started to give nonsense like this
dnl     #define SIZEOF_CHAR 0
dnl   recently.
AC_PREREQ([2.69])

AC_CONFIG_HEADER([ghcautoconf.h.autoconf])

# We have to run these unconditionally, but we may discard their
# results in the following code
AC_CANONICAL_BUILD
AC_CANONICAL_HOST

GHC_CONVERT_PLATFORM_PARTS([host], [Host])
FPTOOLS_SET_PLATFORM_VARS([host],[Host])
FPTOOLS_SET_HASKELL_PLATFORM_VARS([Host])

dnl ** check for mingwex library
AC_CHECK_LIB(
  [mingwex],
  [closedir],
  [AC_SUBST([HaveLibMingwEx],[YES])],
  [AC_SUBST([HaveLibMingwEx],[NO])])

if test $HaveLibMingwEx = YES ; then
  AC_DEFINE([HAVE_MINGWEX], [1], [Define to 1 if you have the mingwex library.])
fi

dnl ** check for math library
dnl    Keep that check as early as possible.
dnl    as we need to know whether we need libm
dnl    for math functions or not
dnl    (see https://gitlab.haskell.org/ghc/ghc/issues/3730)
AC_CHECK_LIB(m, atan, HaveLibM=YES, HaveLibM=NO)
if test $HaveLibM = YES
then
  AC_DEFINE([HAVE_LIBM], [1], [Define to 1 if you need to link with libm])
fi

FP_BFD_SUPPORT

dnl ################################################################
dnl Check for libraries
dnl ################################################################

FP_FIND_LIBFFI

dnl ** check whether we need -ldl to get dlopen()
AC_CHECK_LIB([dl], [dlopen])
dnl ** check whether we have dlinfo
AC_CHECK_FUNCS([dlinfo])

dnl --------------------------------------------------
dnl * Miscellaneous feature tests
dnl --------------------------------------------------

dnl ** can we get alloca?
AC_FUNC_ALLOCA

dnl ** working vfork?
AC_FUNC_FORK

dnl ** determine whether or not const works
AC_C_CONST

dnl ** are we big endian?
AC_C_BIGENDIAN
FPTOOLS_FLOAT_WORD_ORDER_BIGENDIAN

dnl ** check for leading underscores in symbol names
FP_LEADING_UNDERSCORE
if test x"$fptools_cv_leading_underscore" = xyes; then
   AC_DEFINE([LEADING_UNDERSCORE], [1], [Define to 1 if C symbols have a leading underscore added by the compiler.])
fi

FP_VISIBILITY_HIDDEN

dnl ** check for librt
AC_CHECK_LIB([rt], [clock_gettime])
AC_CHECK_FUNCS(clock_gettime timer_settime)
FP_CHECK_TIMER_CREATE

dnl ** check for Apple's "interesting" long double compatibility scheme
AC_MSG_CHECKING(for printf\$LDBLStub)
AC_LINK_IFELSE([AC_LANG_CALL([], [printf\$LDBLStub])],
    [
        AC_MSG_RESULT(yes)
        AC_DEFINE([HAVE_PRINTF_LDBLSTUB],[1],
            [Define to 1 if we have printf$LDBLStub (Apple Mac OS >= 10.4, PPC).])
    ],
    [
        AC_MSG_RESULT(no)
        AC_DEFINE([HAVE_PRINTF_LDBLSTUB],[0],
            [Define to 1 if we have printf$LDBLStub (Apple Mac OS >= 10.4, PPC).])
    ])

FP_CHECK_PTHREADS

dnl ** check for eventfd which is needed by the I/O manager
AC_CHECK_HEADERS([sys/eventfd.h])
AC_CHECK_FUNCS([eventfd])

dnl ** Check for __thread support in the compiler
AC_MSG_CHECKING(for __thread support)
AC_COMPILE_IFELSE(
  [ AC_LANG_SOURCE([[__thread int tester = 0;]]) ],
  [
   AC_MSG_RESULT(yes)
   AC_DEFINE([CC_SUPPORTS_TLS],[1],[Define to 1 if __thread is supported])
  ],
  [
   AC_MSG_RESULT(no)
   AC_DEFINE([CC_SUPPORTS_TLS],[0],[Define to 1 if __thread is supported])
  ])

dnl large address space support (see rts/include/rts/storage/MBlock.h)
dnl
dnl Darwin has vm_allocate/vm_protect
dnl Linux has mmap(MAP_NORESERVE)/madv(MADV_DONTNEED)
dnl FreeBSD, Solaris and maybe other have MAP_NORESERVE/MADV_FREE
dnl (They also have MADV_DONTNEED, but it means something else!)
dnl
dnl Windows has VirtualAlloc MEM_RESERVE/MEM_COMMIT, however
dnl it counts page-table space as committed memory, and so quickly
dnl runs out of paging file when we have multiple processes reserving
dnl 1TB of address space, we get the following error:
dnl    VirtualAlloc MEM_RESERVE 1099512676352 bytes failed: The paging file is too small for this operation to complete.
dnl

AC_ARG_ENABLE(large-address-space,
    [AS_HELP_STRING([--enable-large-address-space],
        [Use a single large address space on 64 bit systems (enabled by default on 64 bit platforms)])],
    EnableLargeAddressSpace=$enableval,
    EnableLargeAddressSpace=yes
)

use_large_address_space=no
if test "$ac_cv_sizeof_void_p" -eq 8 ; then
    if test "x$EnableLargeAddressSpace" = "xyes" ; then
        if test "$ghc_host_os" = "darwin" ; then
            use_large_address_space=yes
        elif test "$ghc_host_os" = "openbsd" ; then
            # as of OpenBSD 5.8 (2015), OpenBSD does not support mmap with MAP_NORESERVE.
            # The flag MAP_NORESERVE is supported for source compatibility reasons,
            # but is completely ignored by OS mmap
                  use_large_address_space=no
        elif test "$ghc_host_os" = "mingw32" ; then
            # as of Windows 8.1/Server 2012 windows does no longer allocate the page
            # tabe for reserved memory eagerly. So we are now free to use LAS there too.
                  use_large_address_space=yes
        else
            AC_CHECK_DECLS([MAP_NORESERVE, MADV_FREE, MADV_DONTNEED],[],[],
                [
                #include <unistd.h>
                #include <sys/types.h>
                #include <sys/mman.h>
                #include <fcntl.h>
            ])
            if test "$ac_cv_have_decl_MAP_NORESERVE" = "yes" &&
                test "$ac_cv_have_decl_MADV_FREE" = "yes" ||
                test "$ac_cv_have_decl_MADV_DONTNEED" = "yes" ; then
                    use_large_address_space=yes
            fi
        fi
    fi
fi
if test "$use_large_address_space" = "yes" ; then
   AC_DEFINE([USE_LARGE_ADDRESS_SPACE], [1], [Enable single heap address space support])
fi

dnl ** Use MMAP in the runtime linker?
dnl --------------------------------------------------------------

case ${HostOS} in
    linux|linux-android|freebsd|dragonfly|netbsd|openbsd|kfreebsdgnu|gnu|solaris2)
        RtsLinkerUseMmap=1
        ;;
    darwin|ios|watchos|tvos)
        RtsLinkerUseMmap=1
        ;;
    *)
        # Windows (which doesn't have mmap) and everything else.
        RtsLinkerUseMmap=0
        ;;
    esac

AC_DEFINE_UNQUOTED([RTS_LINKER_USE_MMAP], [$RtsLinkerUseMmap],
                   [Use mmap in the runtime linker])

GHC_ADJUSTORS_METHOD([Host])
AC_SUBST([UseLibffiForAdjustors])

dnl ** Other RTS features
dnl --------------------------------------------------------------
FP_FIND_LIBDW
if test $UseLibdw = "YES" ; then
  USE_LIBDW=1
else
  USE_LIBDW=0
fi
AC_DEFINE_UNQUOTED([USE_LIBDW], [$USE_LIBDW], [Set to 1 to use libdw])

FP_FIND_LIBNUMA
AC_DEFINE_UNQUOTED([HAVE_LIBNUMA], [$HaveLibNuma], [Define to 1 if you have libnuma])

dnl ** Write config files
dnl --------------------------------------------------------------

AC_OUTPUT

[
mkdir -p include/rts
touch include/ghcautoconf.h
> include/ghcautoconf.h

echo "#if !defined(__GHCAUTOCONF_H__)" >> include/ghcautoconf.h
echo "#define __GHCAUTOCONF_H__" >> include/ghcautoconf.h
# Copy the contents of mk/config.h, turning '#define PACKAGE_FOO
# "blah"' into '/* #undef PACKAGE_FOO */' to avoid clashes.
sed ghcautoconf.h.autoconf \
   -e 's,^\([	 ]*\)#[	 ]*define[	 ][	 ]*\(PACKAGE_[A-Z]*\)[	 ][ 	]*".*".*$,\1/* #undef \2 */,' \
   -e '/__GLASGOW_HASKELL/d' \
   -e '/REMOVE ME/d' \
   >> include/ghcautoconf.h
echo "#endif /* __GHCAUTOCONF_H__ */" >> include/ghcautoconf.h
]
