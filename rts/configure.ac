AC_PREREQ([2.60])
AC_INIT([GHC run-time system], [1.0], [libraries@haskell.org], [rts])

if test ! -f ${srcdir}/includes/ghcautoconf.h.in; then
    echo "${srcdir}/includes/ghcautoconf.h.in doesn't exist: perhaps you haven't run 'python3 boot'?"
    exit 1
fi

# -------------------------------------------------------------------------
# Prepare to generate the following header files
#
#
AC_CONFIG_HEADER(includes/ghcautoconf.h)

GHC_LLVM_TARGET([$target_cpu],[$target_vendor],[$target_os],[LlvmTarget])

# we intend to pass trough --targets to llvm as is.
LLVMTarget_CPP=`    echo "$LlvmTarget"`

AC_SUBST(LLVMTarget_CPP)

FPTOOLS_SET_PLATFORM_VARS([host],[Host])

FPTOOLS_SET_HASKELL_PLATFORM_VARS([Host])
AC_SUBST(HaskellHostArch)
AC_SUBST(HaskellHostOs)

GHC_SUBSECTIONS_VIA_SYMBOLS
AC_SUBST(HasSubsectionsViaSymbols)

GHC_IDENT_DIRECTIVE
AC_SUBST(HasIdentDirective)

GHC_GNU_NONEXEC_STACK
AC_SUBST(HasGnuNonexecStack)

GHC_RTS_LINKER([$host])
AC_SUBST(HasRTSLinker)

dnl ** Used to determine how to compile ghc-prim's atomics.c, used by
dnl    unregisterised, Sparc, and PPC backends.
FP_GCC_SUPPORTS__ATOMICS
AC_DEFINE([HAVE_C11_ATOMICS], [$CONF_GCC_SUPPORTS__ATOMICS], [Does GCC support __atomic primitives?])

dnl ** See whether gcc supports -no-pie
FP_GCC_SUPPORTS_NO_PIE

FP_GCC_EXTRA_FLAGS

dnl --------------------------------------------------
dnl * Platform header file and syscall feature tests
dnl ### checking the state of the local header files and syscalls ###

dnl ** check for full ANSI header (.h) files
AC_HEADER_STDC

dnl ** Enable large file support.  NB. do this before testing the type of
dnl    off_t, because it will affect the result of that test.
AC_SYS_LARGEFILE

dnl ** check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pthread.h pwd.h signal.h sys/param.h sys/mman.h sys/resource.h sys/select.h sys/time.h sys/timeb.h sys/timerfd.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h termios.h time.h utime.h windows.h winsock.h sched.h])

dnl sys/cpuset.h needs sys/param.h to be included first on FreeBSD 9.1; #7708
AC_CHECK_HEADERS([sys/cpuset.h], [], [],
[[#if HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif
]])

dnl ** check if it is safe to include both <time.h> and <sys/time.h>
AC_HEADER_TIME

dnl ** do we have long longs?
AC_CHECK_TYPES([long long])

dnl ** what are the sizes of various types
FP_CHECK_SIZEOF_AND_ALIGNMENT(char)
FP_CHECK_SIZEOF_AND_ALIGNMENT(double)
FP_CHECK_SIZEOF_AND_ALIGNMENT(float)
FP_CHECK_SIZEOF_AND_ALIGNMENT(int)
FP_CHECK_SIZEOF_AND_ALIGNMENT(long)
if test "$ac_cv_type_long_long" = yes; then
FP_CHECK_SIZEOF_AND_ALIGNMENT(long long)
fi
FP_CHECK_SIZEOF_AND_ALIGNMENT(short)
FP_CHECK_SIZEOF_AND_ALIGNMENT(unsigned char)
FP_CHECK_SIZEOF_AND_ALIGNMENT(unsigned int)
FP_CHECK_SIZEOF_AND_ALIGNMENT(unsigned long)
if test "$ac_cv_type_long_long" = yes; then
FP_CHECK_SIZEOF_AND_ALIGNMENT(unsigned long long)
fi
FP_CHECK_SIZEOF_AND_ALIGNMENT(unsigned short)
FP_CHECK_SIZEOF_AND_ALIGNMENT(void *)

FP_CHECK_SIZEOF_AND_ALIGNMENT(int8_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(uint8_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(int16_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(uint16_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(int32_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(uint32_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(int64_t)
FP_CHECK_SIZEOF_AND_ALIGNMENT(uint64_t)


dnl for use in settings file
TargetWordSize=$ac_cv_sizeof_void_p
if test "x$TargetWordSize" == 8; then
  AC_SUBST([Cabal64bit],[True])
else
  AC_SUBST([Cabal64bit],[False])
fi
AC_SUBST(TargetWordSize)
FP_CHECK_FUNC([WinExec],
  [@%:@include <windows.h>], [WinExec("",0)])

FP_CHECK_FUNC([GetModuleFileName],
  [@%:@include <windows.h>], [GetModuleFileName((HMODULE)0,(LPTSTR)0,0)])

dnl ** check for more functions
dnl ** The following have been verified to be used in ghc/, but might be used somewhere else, too.
AC_CHECK_FUNCS([getclock getrusage gettimeofday setitimer siginterrupt sysconf times ctime_r sched_setaffinity setlocale])

dnl ** On OS X 10.4 (at least), time.h doesn't declare ctime_r if
dnl ** _POSIX_C_SOURCE is defined
AC_CHECK_DECLS([ctime_r], , ,
[#define _POSIX_SOURCE 1
#define _POSIX_C_SOURCE 199506L
#include <time.h>])

dnl ** check for mingwex library
AC_CHECK_LIB(
  [mingwex],
  [closedir],
  [AC_SUBST([HaveLibMingwEx],[YES])] [AC_SUBST([CabalMingwex],[True])],
  [AC_SUBST([HaveLibMingwEx],[NO])] [AC_SUBST([CabalMingwex],[False])])

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
  AC_SUBST([CabalHaveLibm],[True])
else
  AC_SUBST([CabalHaveLibm],[False])
fi

FP_BFD_SUPPORT

dnl ################################################################
dnl Check for libraries
dnl ################################################################

# system libffi

AC_ARG_WITH([system-libffi],
[AC_HELP_STRING([--with-system-libffi],
  [Use system provided libffi for RTS [default=no]])
])

AS_IF([test "x$with_system_libffi" = "xyes"],
  [UseSystemLibFFI="YES"], [UseSystemLibFFI="NO"]
)


AC_SUBST(UseSystemLibFFI)

AC_ARG_WITH([ffi-includes],
[AC_HELP_STRING([--with-ffi-includes=ARG],
  [Find includes for libffi in ARG [default=system default]])
],
[
 if test "x$UseSystemLibFFI" != "xYES"; then
    AC_MSG_WARN([--with-ffi-includes will be ignored, --with-system-libffi not set])
 else
    FFIIncludeDir="$withval"
    LIBFFI_CFLAGS="-I$withval"
 fi
])

AC_SUBST(FFIIncludeDir)

AC_ARG_WITH([ffi-libraries],
[AC_HELP_STRING([--with-ffi-libraries=ARG],
  [Find libffi in ARG [default=system default]])
],
[
 if test "x$UseSystemLibFFI" != "xYES"; then
    AC_MSG_WARN([--with-ffi-libraries will be ignored, --with-system-libffi not set])
 else
    FFILibDir="$withval" LIBFFI_LDFLAGS="-L$withval"
 fi
])

AC_SUBST(FFILibDir)

AC_SUBST([CabalHaveLibffi],[False])
AS_IF([test "$UseSystemLibFFI" = "YES"], [
 CFLAGS2="$CFLAGS"
 CFLAGS="$LIBFFI_CFLAGS $CFLAGS"
 LDFLAGS2="$LDFLAGS"
 LDFLAGS="$LIBFFI_LDFLAGS $LDFLAGS"
 if test "$HostOS" = "openbsd";
 then
   # OpenBSD's libffi is not directly linked to the libpthread but
   # still requires pthread functionality. This means that any client
   # binary which links with libffi also needs to link with
   # libpthread. If it does not, then linking fails with unresolved
   # symbols.
   LDFLAGS="$LDFLAGS -lpthread"
 fi
 AC_CHECK_LIB(ffi, ffi_call,
  [AC_CHECK_HEADERS([ffi.h], [break], [])
   AC_DEFINE([HAVE_LIBFFI], [1], [Define to 1 if you have libffi.])]
   AC_SUBST([CabalHaveLibffi],[True]),
  [AC_MSG_ERROR([Cannot find system libffi])])
 CFLAGS="$CFLAGS2"
 LDFLAGS="$LDFLAGS2"
])

dnl ** check whether we need -ldl to get dlopen()
AC_CHECK_LIB([dl], [dlopen])
AC_CHECK_LIB([dl], [dlopen], [AC_SUBST([CabalHaveLibdl], [True])], [AC_SUBST([CabalHaveLibdl], [False])])

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

FP_VISIBILITY_HIDDEN

dnl ** check for librt
AC_CHECK_LIB([rt], [clock_gettime])
AC_CHECK_LIB([rt], [clock_gettime], [AC_SUBST([CabalHaveLibrt], [True])], [AC_SUBST([CabalHaveLibrt], [False])])
AC_CHECK_FUNCS(clock_gettime timer_settime)
FP_CHECK_TIMER_CREATE

dnl ** check for Apple's "interesting" long double compatibility scheme
AC_MSG_CHECKING(for printf\$LDBLStub)
AC_TRY_LINK_FUNC(printf\$LDBLStub,
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

dnl Some platforms (e.g. Android's Bionic) have pthreads support available
dnl without linking against libpthread. Check whether -lpthread is necessary
dnl to use pthreads.
dnl
dnl Note that it is important that this happens before we AC_CHECK_LIB(thread)
AC_MSG_CHECKING(whether -lpthread is needed for pthreads)
AC_CHECK_FUNC(pthread_create,
    [
        AC_MSG_RESULT(no)
        AC_SUBST([CabalNeedLibpthread],[False])
        need_lpthread=0
    ],
    [
        AC_CHECK_LIB(pthread, pthread_create,
            [
                AC_MSG_RESULT(yes)
                AC_SUBST([CabalNeedLibpthread],[True])
                need_lpthread=1
            ],
            [
                AC_SUBST([CabalNeedLibpthread],[False])
                AC_MSG_RESULT([no pthreads support found.])
                need_lpthread=0
            ])
    ])
AC_DEFINE_UNQUOTED([NEED_PTHREAD_LIB], [$need_lpthread],
    [Define 1 if we need to link code using pthreads with -lpthread])

dnl ** pthread_setname_np is a recent addition to glibc, and OS X has
dnl    a different single-argument version.
AC_CHECK_LIB(pthread, pthread_setname_np)
AC_MSG_CHECKING(for pthread_setname_np)
AC_TRY_LINK(
[
#define _GNU_SOURCE
#include <pthread.h>
],
[pthread_setname_np(pthread_self(), "name");],
    AC_MSG_RESULT(yes)
    AC_DEFINE([HAVE_PTHREAD_SETNAME_NP], [1],
      [Define to 1 if you have the glibc version of pthread_setname_np]),
    AC_MSG_RESULT(no)
)

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

dnl large address space support (see includes/rts/storage/MBlock.h)
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
    [AC_HELP_STRING([--enable-large-address-space],
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

case ${TargetOS} in
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

dnl ** Have libdw?
dnl --------------------------------------------------------------
UseLibdw=NO
USE_LIBDW=0
AC_ARG_ENABLE(dwarf-unwind,
    [AC_HELP_STRING([--enable-dwarf-unwind],
        [Enable DWARF unwinding support in the runtime system via elfutils' libdw [default=no]])])
if test "$enable_dwarf_unwind" = "yes" ; then
  AC_CHECK_LIB(dw, dwfl_attach_state,
      [UseLibdw=YES],
      [AC_MSG_ERROR([Cannot find system libdw (required by --enable-dwarf-unwind)])])
fi

AC_SUBST(UseLibdw)
if test $UseLibdw = "YES" ; then
  USE_LIBDW=1
  AC_SUBST([CabalHaveLibdw],[True])
else
  AC_SUBST([CabalHaveLibdw],[False])
fi
AC_DEFINE_UNQUOTED([USE_LIBDW], [$USE_LIBDW], [Set to 1 to use libdw])

dnl ** Have libnuma?
dnl --------------------------------------------------------------
HaveLibNuma=0
AC_ARG_ENABLE(numa,
    [AC_HELP_STRING([--enable-numa],
        [Enable NUMA memory policy and thread affinity support in the
         runtime system via numactl's libnuma [default=auto]])])

if test "$enable_numa" != "no" ; then
  AC_CHECK_HEADERS([numa.h numaif.h])

  if test "$ac_cv_header_numa_h$ac_cv_header_numaif_h" = "yesyes" ; then
    AC_CHECK_LIB(numa, numa_available,HaveLibNuma=1)
  fi
  if test "$enable_numa:$HaveLibNuma" = "yes:0" ; then
      AC_MSG_ERROR([Cannot find system libnuma (required by --enable-numa)])]
  fi
fi

AC_DEFINE_UNQUOTED([HAVE_LIBNUMA], [$HaveLibNuma], [Define to 1 if you have libnuma])
if test $HaveLibNuma = "1" ; then
  AC_SUBST([CabalHaveLibNuma],[True])
else
  AC_SUBST([CabalHaveLibNuma],[False])
fi

AC_CONFIG_FILES([.buildinfo settings])
AC_OUTPUT

dnl ** Create settings file
dnl
dnl This file informs GHC about how to compile code that works
dnl with this RTS build
dnl --------------------------------------------------------------

FP_SETTINGS
