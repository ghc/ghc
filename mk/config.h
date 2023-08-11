/* mk/config.h.  Generated from config.h.in by configure.  */
/* mk/config.h.in.  Generated from configure.ac by autoheader.  */

/* Define if building universal (internal helper macro) */
/* #undef AC_APPLE_UNIVERSAL_BUILD */

/* The alignment of a `char'. */
#define ALIGNMENT_CHAR 1

/* The alignment of a `double'. */
#define ALIGNMENT_DOUBLE 8

/* The alignment of a `float'. */
#define ALIGNMENT_FLOAT 4

/* The alignment of a `int'. */
#define ALIGNMENT_INT 4

/* The alignment of a `int16_t'. */
#define ALIGNMENT_INT16_T 2

/* The alignment of a `int32_t'. */
#define ALIGNMENT_INT32_T 4

/* The alignment of a `int64_t'. */
#define ALIGNMENT_INT64_T 8

/* The alignment of a `int8_t'. */
#define ALIGNMENT_INT8_T 1

/* The alignment of a `long'. */
#define ALIGNMENT_LONG 8

/* The alignment of a `long long'. */
#define ALIGNMENT_LONG_LONG 8

/* The alignment of a `short'. */
#define ALIGNMENT_SHORT 2

/* The alignment of a `uint16_t'. */
#define ALIGNMENT_UINT16_T 2

/* The alignment of a `uint32_t'. */
#define ALIGNMENT_UINT32_T 4

/* The alignment of a `uint64_t'. */
#define ALIGNMENT_UINT64_T 8

/* The alignment of a `uint8_t'. */
#define ALIGNMENT_UINT8_T 1

/* The alignment of a `unsigned char'. */
#define ALIGNMENT_UNSIGNED_CHAR 1

/* The alignment of a `unsigned int'. */
#define ALIGNMENT_UNSIGNED_INT 4

/* The alignment of a `unsigned long'. */
#define ALIGNMENT_UNSIGNED_LONG 8

/* The alignment of a `unsigned long long'. */
#define ALIGNMENT_UNSIGNED_LONG_LONG 8

/* The alignment of a `unsigned short'. */
#define ALIGNMENT_UNSIGNED_SHORT 2

/* The alignment of a `void *'. */
#define ALIGNMENT_VOID_P 8

/* Define (to 1) if C compiler has an LLVM back end */
/* #undef CC_LLVM_BACKEND */

/* Define to 1 if __thread is supported */
#define CC_SUPPORTS_TLS 1

/* Define to 1 if using 'alloca.c'. */
/* #undef C_ALLOCA */

/* Enable Native I/O manager as default. */
/* #undef DEFAULT_NATIVE_IO_MANAGER */

/* Define to 1 if your processor stores words of floats with the most
   significant byte first */
/* #undef FLOAT_WORDS_BIGENDIAN */

/* Has musttail */
/* #undef HAS_MUSTTAIL */

/* Has visibility hidden */
#define HAS_VISIBILITY_HIDDEN 1

/* Define to 1 if you have 'alloca', as a function or macro. */
#define HAVE_ALLOCA 1

/* Define to 1 if <alloca.h> works. */
#define HAVE_ALLOCA_H 1

/* Define to 1 if you have the <bfd.h> header file. */
/* #undef HAVE_BFD_H */

/* Does C compiler support __atomic primitives? */
#define HAVE_C11_ATOMICS 1

/* Define to 1 if you have the 'clock_gettime' function. */
#define HAVE_CLOCK_GETTIME 1

/* Define to 1 if you have the 'ctime_r' function. */
#define HAVE_CTIME_R 1

/* Define to 1 if you have the <ctype.h> header file. */
#define HAVE_CTYPE_H 1

/* Define to 1 if you have the declaration of 'ctime_r', and to 0 if you
   don't. */
#define HAVE_DECL_CTIME_R 1

/* Define to 1 if you have the declaration of 'environ', and to 0 if you
   don't. */
#define HAVE_DECL_ENVIRON 1

/* Define to 1 if you have the declaration of 'MADV_DONTNEED', and to 0 if you
   don't. */
#define HAVE_DECL_MADV_DONTNEED 1

/* Define to 1 if you have the declaration of 'MADV_FREE', and to 0 if you
   don't. */
#define HAVE_DECL_MADV_FREE 1

/* Define to 1 if you have the declaration of 'MAP_NORESERVE', and to 0 if you
   don't. */
#define HAVE_DECL_MAP_NORESERVE 1

/* Define to 1 if you have the declaration of 'program_invocation_short_name',
   and to 0 if you don't. */
#define HAVE_DECL_PROGRAM_INVOCATION_SHORT_NAME 1

/* Define to 1 if you have the <dirent.h> header file. */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the 'dlinfo' function. */
#define HAVE_DLINFO 1

/* Define to 1 if you have the <elfutils/libdw.h> header file. */
#define HAVE_ELFUTILS_LIBDW_H 1

/* Define to 1 if you have the <errno.h> header file. */
#define HAVE_ERRNO_H 1

/* Define to 1 if you have the 'eventfd' function. */
#define HAVE_EVENTFD 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the <ffi.h> header file. */
/* #undef HAVE_FFI_H */

/* Define to 1 if you have the 'fork' function. */
#define HAVE_FORK 1

/* Define to 1 if you have the 'getclock' function. */
/* #undef HAVE_GETCLOCK */

/* Define to 1 if you have the `GetModuleFileName' function. */
/* #undef HAVE_GETMODULEFILENAME */

/* Define to 1 if you have the 'getpid' function. */
#define HAVE_GETPID 1

/* Define to 1 if you have the 'getrusage' function. */
#define HAVE_GETRUSAGE 1

/* Define to 1 if you have the 'gettimeofday' function. */
#define HAVE_GETTIMEOFDAY 1

/* Define to 1 if you have the 'getuid' function. */
#define HAVE_GETUID 1

/* Define to 1 if you have the <grp.h> header file. */
#define HAVE_GRP_H 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the 'bfd' library (-lbfd). */
/* #undef HAVE_LIBBFD */

/* Define to 1 if you have the 'dl' library (-ldl). */
#define HAVE_LIBDL 1

/* Define to 1 if you have the 'iberty' library (-liberty). */
/* #undef HAVE_LIBIBERTY */

/* Define to 1 if you need to link with libm */
#define HAVE_LIBM 1

/* Define to 1 if you have the 'mingwex' library (-lmingwex). */
/* #undef HAVE_LIBMINGWEX */

/* Define to 1 if you have libnuma */
#define HAVE_LIBNUMA 0

/* Define to 1 if you have the 'pthread' library (-lpthread). */
#define HAVE_LIBPTHREAD 1

/* Define to 1 if you have the 'rt' library (-lrt). */
#define HAVE_LIBRT 1

/* Define to 1 if you wish to compress IPE data in compiler results (requires
   libzstd) */
#define HAVE_LIBZSTD 0

/* Define to 1 if you have the <limits.h> header file. */
#define HAVE_LIMITS_H 1

/* Define to 1 if you have the <locale.h> header file. */
#define HAVE_LOCALE_H 1

/* Define to 1 if the system has the type 'long long'. */
#define HAVE_LONG_LONG 1

/* Define to 1 if you have the <minix/config.h> header file. */
/* #undef HAVE_MINIX_CONFIG_H */

/* Define to 1 if you have the <nlist.h> header file. */
#define HAVE_NLIST_H 1

/* Define to 1 if you have the <numaif.h> header file. */
/* #undef HAVE_NUMAIF_H */

/* Define to 1 if you have the <numa.h> header file. */
/* #undef HAVE_NUMA_H */

/* Define to 1 if we have printf$LDBLStub (Apple Mac OS >= 10.4, PPC). */
#define HAVE_PRINTF_LDBLSTUB 0

/* Define to 1 if you have the 'pthread_condattr_setclock' function. */
#define HAVE_PTHREAD_CONDATTR_SETCLOCK 1

/* Define to 1 if you have the <pthread.h> header file. */
#define HAVE_PTHREAD_H 1

/* Define to 1 if you have the <pthread_np.h> header file. */
/* #undef HAVE_PTHREAD_NP_H */

/* Define to 1 if you have the glibc version of pthread_setname_np */
#define HAVE_PTHREAD_SETNAME_NP 1

/* Define to 1 if you have the Darwin version of pthread_setname_np */
/* #undef HAVE_PTHREAD_SETNAME_NP_DARWIN */

/* Define to 1 if you have the NetBSD version of pthread_setname_np */
/* #undef HAVE_PTHREAD_SETNAME_NP_NETBSD */

/* Define to 1 if you have pthread_set_name_np */
/* #undef HAVE_PTHREAD_SET_NAME_NP */

/* Define to 1 if you have the <pwd.h> header file. */
#define HAVE_PWD_H 1

/* Define to 1 if you have the 'raise' function. */
#define HAVE_RAISE 1

/* Define to 1 if you have the 'sched_getaffinity' function. */
#define HAVE_SCHED_GETAFFINITY 1

/* Define to 1 if you have the <sched.h> header file. */
#define HAVE_SCHED_H 1

/* Define to 1 if you have the 'sched_setaffinity' function. */
#define HAVE_SCHED_SETAFFINITY 1

/* Define to 1 if you have the 'setitimer' function. */
#define HAVE_SETITIMER 1

/* Define to 1 if you have the 'setlocale' function. */
#define HAVE_SETLOCALE 1

/* Define to 1 if you have the 'siginterrupt' function. */
#define HAVE_SIGINTERRUPT 1

/* Define to 1 if you have the <signal.h> header file. */
#define HAVE_SIGNAL_H 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdio.h> header file. */
#define HAVE_STDIO_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if Apple-style dead-stripping is supported. */
/* #undef HAVE_SUBSECTIONS_VIA_SYMBOLS */

/* Define to 1 if you have the 'sysconf' function. */
#define HAVE_SYSCONF 1

/* Define to 1 if you have libffi. */
/* #undef HAVE_SYSTEM_LIBFFI */

/* Define to 1 if you have the <sys/cpuset.h> header file. */
/* #undef HAVE_SYS_CPUSET_H */

/* Define to 1 if you have the <sys/eventfd.h> header file. */
#define HAVE_SYS_EVENTFD_H 1

/* Define to 1 if you have the <sys/mman.h> header file. */
#define HAVE_SYS_MMAN_H 1

/* Define to 1 if you have the <sys/param.h> header file. */
#define HAVE_SYS_PARAM_H 1

/* Define to 1 if you have the <sys/resource.h> header file. */
#define HAVE_SYS_RESOURCE_H 1

/* Define to 1 if you have the <sys/select.h> header file. */
#define HAVE_SYS_SELECT_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/timeb.h> header file. */
#define HAVE_SYS_TIMEB_H 1

/* Define to 1 if you have the <sys/timerfd.h> header file. */
#define HAVE_SYS_TIMERFD_H 1

/* Define to 1 if you have the <sys/timers.h> header file. */
/* #undef HAVE_SYS_TIMERS_H */

/* Define to 1 if you have the <sys/times.h> header file. */
#define HAVE_SYS_TIMES_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <sys/utsname.h> header file. */
#define HAVE_SYS_UTSNAME_H 1

/* Define to 1 if you have the <sys/wait.h> header file. */
#define HAVE_SYS_WAIT_H 1

/* Define to 1 if you have the <termios.h> header file. */
#define HAVE_TERMIOS_H 1

/* Define to 1 if you have the 'timer_settime' function. */
#define HAVE_TIMER_SETTIME 1

/* Define to 1 if you have the 'times' function. */
#define HAVE_TIMES 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the 'uselocale' function. */
#define HAVE_USELOCALE 1

/* Define to 1 if you have the <utime.h> header file. */
#define HAVE_UTIME_H 1

/* Define to 1 if you have the 'vfork' function. */
#define HAVE_VFORK 1

/* Define to 1 if you have the <vfork.h> header file. */
/* #undef HAVE_VFORK_H */

/* Define to 1 if you have the <wchar.h> header file. */
#define HAVE_WCHAR_H 1

/* Define to 1 if you have the <windows.h> header file. */
/* #undef HAVE_WINDOWS_H */

/* Define to 1 if you have the `WinExec' function. */
/* #undef HAVE_WINEXEC */

/* Define to 1 if you have the <winsock.h> header file. */
/* #undef HAVE_WINSOCK_H */

/* Define to 1 if 'fork' works. */
#define HAVE_WORKING_FORK 1

/* Define to 1 if 'vfork' works. */
#define HAVE_WORKING_VFORK 1

/* Define to 1 if you have the <zstd.h> header file. */
/* #undef HAVE_ZSTD_H */

/* Define to 1 if C symbols have a leading underscore added by the compiler.
   */
/* #undef LEADING_UNDERSCORE */

/* Define to 1 if we need -latomic. */
#define NEED_ATOMIC_LIB 0

/* Define 1 if we need to link code using pthreads with -lpthread */
#define NEED_PTHREAD_LIB 0

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "glasgow-haskell-bugs@haskell.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "The Glorious Glasgow Haskell Compilation System"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "The Glorious Glasgow Haskell Compilation System 9.9"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "ghc-9.9"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "9.9"

/* Use mmap in the runtime linker */
#define RTS_LINKER_USE_MMAP 1

/* The size of 'char', as computed by sizeof. */
#define SIZEOF_CHAR 1

/* The size of 'double', as computed by sizeof. */
#define SIZEOF_DOUBLE 8

/* The size of 'float', as computed by sizeof. */
#define SIZEOF_FLOAT 4

/* The size of 'int', as computed by sizeof. */
#define SIZEOF_INT 4

/* The size of 'int16_t', as computed by sizeof. */
#define SIZEOF_INT16_T 2

/* The size of 'int32_t', as computed by sizeof. */
#define SIZEOF_INT32_T 4

/* The size of 'int64_t', as computed by sizeof. */
#define SIZEOF_INT64_T 8

/* The size of 'int8_t', as computed by sizeof. */
#define SIZEOF_INT8_T 1

/* The size of 'long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of 'long long', as computed by sizeof. */
#define SIZEOF_LONG_LONG 8

/* The size of 'short', as computed by sizeof. */
#define SIZEOF_SHORT 2

/* The size of 'uint16_t', as computed by sizeof. */
#define SIZEOF_UINT16_T 2

/* The size of 'uint32_t', as computed by sizeof. */
#define SIZEOF_UINT32_T 4

/* The size of 'uint64_t', as computed by sizeof. */
#define SIZEOF_UINT64_T 8

/* The size of 'uint8_t', as computed by sizeof. */
#define SIZEOF_UINT8_T 1

/* The size of 'unsigned char', as computed by sizeof. */
#define SIZEOF_UNSIGNED_CHAR 1

/* The size of 'unsigned int', as computed by sizeof. */
#define SIZEOF_UNSIGNED_INT 4

/* The size of 'unsigned long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG 8

/* The size of 'unsigned long long', as computed by sizeof. */
#define SIZEOF_UNSIGNED_LONG_LONG 8

/* The size of 'unsigned short', as computed by sizeof. */
#define SIZEOF_UNSIGNED_SHORT 2

/* The size of 'void *', as computed by sizeof. */
#define SIZEOF_VOID_P 8

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
	STACK_DIRECTION > 0 => grows toward higher addresses
	STACK_DIRECTION < 0 => grows toward lower addresses
	STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if you wish to statically link the libzstd compression library
   in the compiler (requires libzstd) */
#define STATIC_LIBZSTD 0

/* Define to 1 if all of the C89 standard headers exist (not just the ones
   required in a freestanding environment). This macro is provided for
   backward compatibility; new code need not use it. */
#define STDC_HEADERS 1

/* Define to 1 if info tables are laid out next to code */
#define TABLES_NEXT_TO_CODE 1

/* Compile-in ASSERTs in all ways. */
/* #undef USE_ASSERTS_ALL_WAYS */

/* Enable single heap address space support */
#define USE_LARGE_ADDRESS_SPACE 1

/* Set to 1 to use libdw */
#define USE_LIBDW 1

/* Enable extensions on AIX, Interix, z/OS.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable general extensions on macOS.  */
#ifndef _DARWIN_C_SOURCE
# define _DARWIN_C_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable X/Open compliant socket functions that do not require linking
   with -lxnet on HP-UX 11.11.  */
#ifndef _HPUX_ALT_XOPEN_SOCKET_API
# define _HPUX_ALT_XOPEN_SOCKET_API 1
#endif
/* Identify the host operating system as Minix.
   This macro does not affect the system headers' behavior.
   A future release of Autoconf may stop defining this macro.  */
#ifndef _MINIX
/* # undef _MINIX */
#endif
/* Enable general extensions on NetBSD.
   Enable NetBSD compatibility extensions on Minix.  */
#ifndef _NETBSD_SOURCE
# define _NETBSD_SOURCE 1
#endif
/* Enable OpenBSD compatibility extensions on NetBSD.
   Oddly enough, this does nothing on OpenBSD.  */
#ifndef _OPENBSD_SOURCE
# define _OPENBSD_SOURCE 1
#endif
/* Define to 1 if needed for POSIX-compatible behavior.  */
#ifndef _POSIX_SOURCE
/* # undef _POSIX_SOURCE */
#endif
/* Define to 2 if needed for POSIX-compatible behavior.  */
#ifndef _POSIX_1_SOURCE
/* # undef _POSIX_1_SOURCE */
#endif
/* Enable POSIX-compatible threading on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions specified by ISO/IEC TS 18661-5:2014.  */
#ifndef __STDC_WANT_IEC_60559_ATTRIBS_EXT__
# define __STDC_WANT_IEC_60559_ATTRIBS_EXT__ 1
#endif
/* Enable extensions specified by ISO/IEC TS 18661-1:2014.  */
#ifndef __STDC_WANT_IEC_60559_BFP_EXT__
# define __STDC_WANT_IEC_60559_BFP_EXT__ 1
#endif
/* Enable extensions specified by ISO/IEC TS 18661-2:2015.  */
#ifndef __STDC_WANT_IEC_60559_DFP_EXT__
# define __STDC_WANT_IEC_60559_DFP_EXT__ 1
#endif
/* Enable extensions specified by C23 Annex F.  */
#ifndef __STDC_WANT_IEC_60559_EXT__
# define __STDC_WANT_IEC_60559_EXT__ 1
#endif
/* Enable extensions specified by ISO/IEC TS 18661-4:2015.  */
#ifndef __STDC_WANT_IEC_60559_FUNCS_EXT__
# define __STDC_WANT_IEC_60559_FUNCS_EXT__ 1
#endif
/* Enable extensions specified by C23 Annex H and ISO/IEC TS 18661-3:2015.  */
#ifndef __STDC_WANT_IEC_60559_TYPES_EXT__
# define __STDC_WANT_IEC_60559_TYPES_EXT__ 1
#endif
/* Enable extensions specified by ISO/IEC TR 24731-2:2010.  */
#ifndef __STDC_WANT_LIB_EXT2__
# define __STDC_WANT_LIB_EXT2__ 1
#endif
/* Enable extensions specified by ISO/IEC 24747:2009.  */
#ifndef __STDC_WANT_MATH_SPEC_FUNCS__
# define __STDC_WANT_MATH_SPEC_FUNCS__ 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable X/Open extensions.  Define to 500 only if necessary
   to make mbstate_t available.  */
#ifndef _XOPEN_SOURCE
/* # undef _XOPEN_SOURCE */
#endif


/* Define to 1 if we can use timer_create(CLOCK_REALTIME,...) */
#define USE_TIMER_CREATE 1

/* Define WORDS_BIGENDIAN to 1 if your processor stores words with the most
   significant byte first (like Motorola and SPARC, unlike Intel). */
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
#  define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
/* #  undef WORDS_BIGENDIAN */
# endif
#endif

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */

/* Define to 1 on platforms where this makes off_t a 64-bit type. */
/* #undef _LARGE_FILES */

/* Number of bits in time_t, on hosts where this is settable. */
/* #undef _TIME_BITS */

/* Define to 1 on platforms where this makes time_t a 64-bit type. */
/* #undef __MINGW_USE_VC2005_COMPAT */

/* ARM pre v6 */
/* #undef arm_HOST_ARCH_PRE_ARMv6 */

/* ARM pre v7 */
/* #undef arm_HOST_ARCH_PRE_ARMv7 */

/* Define to empty if 'const' does not conform to ANSI C. */
/* #undef const */

/* Define as a signed integer type capable of holding a process identifier. */
/* #undef pid_t */

/* The maximum supported LLVM version number */
#define sUPPORTED_LLVM_VERSION_MAX (16)

/* The minimum supported LLVM version number */
#define sUPPORTED_LLVM_VERSION_MIN (11)

/* Define as 'unsigned int' if <stddef.h> doesn't define. */
/* #undef size_t */

/* Define as 'fork' if 'vfork' does not work. */
/* #undef vfork */
