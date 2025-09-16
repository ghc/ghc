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

AC_CONFIG_FILES([ghcplatform.h.top])

AC_CONFIG_HEADERS([ghcautoconf.h.autoconf])

AC_ARG_ENABLE(asserts-all-ways,
[AS_HELP_STRING([--enable-asserts-all-ways],
                [Usually ASSERTs are only compiled in the DEBUG way,
                 this will enable them in all ways.])],
  [FP_CAPITALIZE_YES_NO(["$enableval"], [EnableAssertsAllWays])],
  [EnableAssertsAllWays=NO]
)
if test "$enable_asserts_all_ways" = "yes" ; then
   AC_DEFINE([USE_ASSERTS_ALL_WAYS], [1], [Compile-in ASSERTs in all ways.])
fi

# We have to run these unconditionally, but we may discard their
# results in the following code
AC_CANONICAL_BUILD
AC_CANONICAL_HOST

dnl ** Do a build with tables next to code?
dnl --------------------------------------------------------------

AS_IF(
  [test "$CABAL_FLAG_tables_next_to_code" = 1],
  [AC_DEFINE([TABLES_NEXT_TO_CODE], [1], [Define to 1 if info tables are laid out next to code])])

dnl detect compiler (prefer gcc over clang) and set $CC (unless CC already set),
dnl later CC is copied to CC_STAGE{1,2,3}
AC_PROG_CC([cc gcc clang])

dnl make extensions visible to allow feature-tests to detect them lateron
AC_USE_SYSTEM_EXTENSIONS

dnl ** Used to determine how to compile ghc-internal's atomics.c, used by
dnl    unregisterised, Sparc, and PPC backends. Also determines whether
dnl    linking to libatomic is required for atomic operations, e.g. on
dnl    RISCV64 GCC.
FP_CC_SUPPORTS__ATOMICS
AC_DEFINE_UNQUOTED([NEED_ATOMIC_LIB], [$need_latomic],
    [Define to 1 if we need -latomic for sub-word atomic operations.])

dnl ** look to see if we have a C compiler using an llvm back end.
dnl
FP_CC_LLVM_BACKEND
AS_IF([test x"$CcLlvmBackend" = x"YES"],
  [AC_DEFINE([CC_LLVM_BACKEND], [1], [Define (to 1) if C compiler has an LLVM back end])])

GHC_CONVERT_PLATFORM_PARTS([build], [Build])
FPTOOLS_SET_PLATFORM_VARS([build],[Build])
FPTOOLS_SET_HASKELL_PLATFORM_VARS([Build])

GHC_CONVERT_PLATFORM_PARTS([host], [Host])
FPTOOLS_SET_PLATFORM_VARS([host], [Host])
FPTOOLS_SET_HASKELL_PLATFORM_VARS([Host])

GHC_SUBSECTIONS_VIA_SYMBOLS
AS_IF([test x"${TargetHasSubsectionsViaSymbols}" = x"YES"],
  [AC_DEFINE([HAVE_SUBSECTIONS_VIA_SYMBOLS],[1],
    [Define to 1 if Apple-style dead-stripping is supported.])])

GHC_GNU_NONEXEC_STACK
AS_IF([test x"$TargetHasGnuNonexecStack" = x"YES"],
  [AC_DEFINE([HAVE_GNU_NONEXEC_STACK], [1], [Define (to 1) if GNU-style non-executable stack note is supported])])

dnl --------------------------------------------------
dnl * Platform header file and syscall feature tests
dnl ### checking the state of the local header files and syscalls ###

dnl ** Enable large file support.  NB. do this before testing the type of
dnl    off_t, because it will affect the result of that test.
AC_SYS_LARGEFILE

dnl ** check for specific header (.h) files that we are interested in
AC_CHECK_HEADERS([ctype.h dirent.h dlfcn.h errno.h fcntl.h grp.h limits.h locale.h nlist.h pthread.h pwd.h signal.h sys/param.h sys/mman.h sys/resource.h sys/select.h sys/time.h sys/timeb.h sys/timerfd.h sys/timers.h sys/times.h sys/utsname.h sys/wait.h termios.h utime.h windows.h winsock.h sched.h])

dnl sys/cpuset.h needs sys/param.h to be included first on FreeBSD 9.1; #7708
AC_CHECK_HEADERS([sys/cpuset.h], [], [],
[[#if HAVE_SYS_PARAM_H
# include <sys/param.h>
#endif
]])

dnl ** check whether a declaration for `environ` is provided by libc.
FP_CHECK_ENVIRON

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


FP_CHECK_FUNC([WinExec],
  [@%:@include <windows.h>], [WinExec("",0)])

FP_CHECK_FUNC([GetModuleFileName],
  [@%:@include <windows.h>], [GetModuleFileName((HMODULE)0,(LPTSTR)0,0)])

dnl ** check for more functions
dnl ** The following have been verified to be used in ghc/, but might be used somewhere else, too.
AC_CHECK_FUNCS([getclock getrusage gettimeofday setitimer siginterrupt sysconf times ctime_r sched_setaffinity sched_getaffinity setlocale uselocale])

dnl ** On OS X 10.4 (at least), time.h doesn't declare ctime_r if
dnl ** _POSIX_C_SOURCE is defined
AC_CHECK_DECLS([ctime_r], , ,
[#define _POSIX_SOURCE 1
#define _POSIX_C_SOURCE 199506L
#include <time.h>])

dnl On Linux we should have program_invocation_short_name
AC_CHECK_DECLS([program_invocation_short_name], , ,
[#define _GNU_SOURCE 1
#include <errno.h>])

dnl ** check for math library
dnl    Keep that check as early as possible.
dnl    as we need to know whether we need libm
dnl    for math functions or not
dnl    (see https://gitlab.haskell.org/ghc/ghc/issues/3730)
AS_IF(
  [test "$CABAL_FLAG_libm" = 1],
  [AC_DEFINE([HAVE_LIBM], [1], [Define to 1 if you need to link with libm])])

AS_IF([test "$CABAL_FLAG_libbfd" = 1], [FP_WHEN_ENABLED_BFD])

dnl ################################################################
dnl Check for libraries
dnl ################################################################

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
if test "$CABAL_FLAG_leading_underscore" = 1; then
   AC_DEFINE([LEADING_UNDERSCORE], [1], [Define to 1 if C symbols have a leading underscore added by the compiler.])
fi

FP_VISIBILITY_HIDDEN

FP_MUSTTAIL

dnl ** check for librt
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

FP_CHECK_PTHREAD_FUNCS

dnl ** check for eventfd which is needed by the I/O manager
AC_CHECK_HEADERS([sys/eventfd.h])
AC_CHECK_FUNCS([eventfd])

AC_CHECK_FUNCS([getpid getuid raise])

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

dnl ** ARM outline atomics
dnl --------------------------------------------------------------
FP_ARM_OUTLINE_ATOMICS

dnl ** IPE data compression
dnl --------------------------------------------------------------
AC_DEFINE_UNQUOTED([HAVE_LIBZSTD], [$CABAL_FLAG_libzstd], [Define to 1 if you
  wish to compress IPE data in compiler results (requires libzstd)])

AC_DEFINE_UNQUOTED([STATIC_LIBZSTD], [$CABAL_FLAG_static_libzstd], [Define to 1 if you
  wish to statically link the libzstd compression library in the compiler
  (requires libzstd)])

dnl ** Other RTS features
dnl --------------------------------------------------------------
AC_DEFINE_UNQUOTED([USE_LIBDW], [$CABAL_FLAG_libdw], [Set to 1 to use libdw])

AC_DEFINE_UNQUOTED([HAVE_LIBNUMA], [$CABAL_FLAG_libnuma], [Define to 1 if you have libnuma])
dnl We should have these headers if the flag is set, but check anyways
dnl in order to define `HAVE_*` macros.
AS_IF(
  [test "$CABAL_FLAG_libnuma" = 1],
  [AC_CHECK_HEADERS([numa.h numaif.h])])


dnl ** I/O managers
dnl --------------------------------------------------------------
dnl
dnl The scheme here is that every I/O manager can be enabled/disabled
dnl at GHC build time (subject to some constraints). More than one I/O
dnl manager can be enabled to be built. At least one I/O manager
dnl supporting the threaded RTS must be enabled as well as at least
dnl one supporting the non-threaded RTS. The I/O managers enabled here
dnl become the choices available at runtime at RTS startup. The choice
dnl can be made with RTS flags. There are separate choices for the
dnl threaded and non-threaded RTS ways, because most I/O managers are
dnl specific to these ways. Furthermore we must establish a default I/O
dnl manager for the threaded and non-threaded RTS.
dnl
dnl Most I/O managers are platform-specific so there are checks to
dnl ensure each one can be enabled on the platform. Such checks are
dnl also where any system dependencies (e.g. libraries) can be checked.
dnl
dnl The output is a set of CPP flags, with one flag per named I/O manager:
dnl * IOMGR_BUILD_<name>                : which ones should be built (some)
dnl * IOMGR_DEFAULT_NON_THREADED_<name> : which one is default (exactly one)
dnl * IOMGR_DEFAULT_THREADED_<name>     : which one is default (exactly one)
dnl
dnl Note that IOMGR_BUILD_<name> just says that an I/O manager will be
dnl built for _some_ RTS way (i.e. threaded or non-threaded). There is
dnl a set of derived flags IOMGR_ENABLED_<name> in IOManager.h which says
dnl if each I/O manager is enabled in the "current" RTS way. These are
dnl the ones used for conditional compilation of the I/O manager code.
dnl -------------------------------------------------------------------

dnl Here we check for platform constraints. The result is the CPP flag
dnl IOMGR_BUILD_<name> and a EnableIOManager<Name> var for use here later.

GHC_IOMANAGER_ENABLE([select], [EnableIOManagerSelect], [IOMGR_BUILD_SELECT],
  [if test "$HostOS" = "mingw32"; then
       EnableIOManagerSelect=NO
   else
       AC_CHECK_HEADER([sys/select.h],
           [EnableIOManagerSelect=YES],
           [AC_MSG_ERROR([sys/select.h required by select I/O manager])],[])
   fi])

GHC_IOMANAGER_ENABLE([mio], [EnableIOManagerMIO], [IOMGR_BUILD_MIO],
  [EnableIOManagerMIO=YES])

GHC_IOMANAGER_ENABLE([win32-legacy], [EnableIOManagerWin32Legacy], [IOMGR_BUILD_WIN32_LEGACY],
  [if test "$HostOS" = "mingw32"; then EnableIOManagerWin32Legacy=YES; fi])

GHC_IOMANAGER_ENABLE([winio], [EnableIOManagerWinIO], [IOMGR_BUILD_WINIO],
  [if test "$HostOS" = "mingw32"; then EnableIOManagerWinIO=YES; fi])

dnl Now we establish a default I/O manager for the threaded and non-threaded
dnl RTS. We select the default based on which I/O managers are enabled. They
dnl are checked in reverse order of priority, the last enabled one wins:
if test "$HostOS" = "mingw32"; then
  GHC_IOMANAGER_DEFAULT_SELECT([IOManagerNonThreadedDefault], [winio], [EnableIOManagerWinIO],)
  GHC_IOMANAGER_DEFAULT_SELECT([IOManagerNonThreadedDefault], [win32-legacy], [EnableIOManagerWin32Legacy])
  GHC_IOMANAGER_DEFAULT_SELECT([IOManagerThreadedDefault], [winio], [EnableIOManagerWinIO])
  GHC_IOMANAGER_DEFAULT_SELECT([IOManagerThreadedDefault], [mio], [EnableIOManagerMIO])
else
  GHC_IOMANAGER_DEFAULT_SELECT([IOManagerNonThreadedDefault], [select], [EnableIOManagerSelect])
  GHC_IOMANAGER_DEFAULT_SELECT([IOManagerThreadedDefault], [mio], [EnableIOManagerMIO])
fi
GHC_IOMANAGER_DEFAULT_CHECK_NOT_EMPTY([IOManagerNonThreadedDefault],[non-threaded])
GHC_IOMANAGER_DEFAULT_CHECK_NOT_EMPTY([IOManagerThreadedDefault],[threaded])

AC_MSG_NOTICE(default I/O manager for the non-threaded RTS: ${IOManagerNonThreadedDefault})
AC_MSG_NOTICE(default I/O manager for the threaded RTS: ${IOManagerThreadedDefault})

dnl Now define CPP vars for the default ones (threaded and non-threaded)
GHC_IOMANAGER_DEFAULT_AC_DEFINE([IOManagerNonThreadedDefault], [non-threaded],
                                [select], [IOMGR_DEFAULT_NON_THREADED_SELECT])

GHC_IOMANAGER_DEFAULT_AC_DEFINE([IOManagerNonThreadedDefault], [non-threaded],
                                [winio], [IOMGR_DEFAULT_NON_THREADED_WINIO])

GHC_IOMANAGER_DEFAULT_AC_DEFINE([IOManagerNonThreadedDefault], [non-threaded],
                                [win32-legacy], [IOMGR_DEFAULT_NON_THREADED_WIN32_LEGACY])

GHC_IOMANAGER_DEFAULT_AC_DEFINE([IOManagerThreadedDefault], [threaded],
                                [mio], [IOMGR_DEFAULT_THREADED_MIO])

GHC_IOMANAGER_DEFAULT_AC_DEFINE([IOManagerThreadedDefault], [threaded],
                                [winio], [IOMGR_DEFAULT_THREADED_WINIO])


dnl ** Write config files
dnl --------------------------------------------------------------

AC_OUTPUT

dnl ######################################################################
dnl Generate ghcplatform.h
dnl ######################################################################

[
mkdir -p include

touch include/ghcplatform.h
> include/ghcplatform.h

cat ghcplatform.h.top                          >> include/ghcplatform.h
]

dnl ** Do an unregisterised build?
dnl --------------------------------------------------------------
AS_IF(
  [test "$CABAL_FLAG_unregisterised" = 1],
  [echo "#define UnregisterisedCompiler 1"     >> include/ghcplatform.h])

[
cat $srcdir/ghcplatform.h.bottom               >> include/ghcplatform.h
]

dnl ######################################################################
dnl Generate ghcautoconf.h
dnl ######################################################################

[
touch include/ghcautoconf.h
> include/ghcautoconf.h

echo "#if !defined(__GHCAUTOCONF_H__)" >> include/ghcautoconf.h
echo "#define __GHCAUTOCONF_H__" >> include/ghcautoconf.h
# Copy the contents of ghcautoconf.h.autoconf, turning '#define PACKAGE_FOO
# "blah"' into '/* #undef PACKAGE_FOO */' to avoid clashes.
cat ghcautoconf.h.autoconf | sed \
   -e 's,^\([	 ]*\)#[	 ]*define[	 ][	 ]*\(PACKAGE_[A-Z]*\)[	 ][ 	]*".*".*$,\1/* #undef \2 */,' \
   -e '/__GLASGOW_HASKELL/d' \
   -e '/REMOVE ME/d' \
   >> include/ghcautoconf.h
echo "#endif /* __GHCAUTOCONF_H__ */" >> include/ghcautoconf.h
]

dnl ######################################################################
dnl Generate external symbol flags (-Wl,-u...)
dnl ######################################################################

dnl See Note [Undefined symbols in the RTS]

[
symbolExtraDefs=''
if [ "$CABAL_FLAG_find_ptr" = 1 ]; then
    symbolExtraDefs+=' -DFIND_PTR'
fi

cat $srcdir/external-symbols.list.in \
    | "$CC" $symbolExtraDefs -E -P -traditional -Iinclude - -o - \
    | sed -e '/^ *$/d' \
    > external-symbols.list \
    || exit 1

if [ "$CABAL_FLAG_leading_underscore" = 1 ]; then
    sedExpr='s/^(.*)$/  "-Wl,-u,_\1"/'
else
    sedExpr='s/^(.*)$/  "-Wl,-u,\1"/'
fi
sed -E -e "${sedExpr}" external-symbols.list > external-symbols.flags
unset sedExpr
rm -f external-symbols.list
]

dnl ######################################################################
dnl Generate build-info
dnl ######################################################################

[
cat $srcdir/rts.buildinfo.in \
    | "$CC" -E -P -traditional - -o - \
    | sed -e '/^ *$/d' \
    > rts.buildinfo \
    || exit 1
rm -f external-symbols.flags
]
