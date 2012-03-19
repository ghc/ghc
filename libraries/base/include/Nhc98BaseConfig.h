/* -----------------------------------------------------------------------------
 * (c) Malcolm Wallace 2009
 * ---------------------------------------------------------------------------*/

/* Until we work out how to use a configure script to determine the available
   system headers (without going down the horrid autoconf route), let's just
   take a reasonable hard-coded guess for now.  Feel free to change these
   for your platform.
 */
#define HAVE_SYS_TYPES_H	1
#define HAVE_UNISTD_H		1
#define HAVE_SYS_STAT_H		1
#define HAVE_FCNTL_H		1
#define HAVE_TERMIOS_H		1
#define HAVE_SIGNAL_H		1
#define HAVE_ERRNO_H		1
#define HAVE_STRING_H		1
#define HAVE_UTIME_H		1
#define HAVE_SYS_UTSNAME_H	1
#define HAVE_GETTIMEOFDAY	1
#define HAVE_SYS_TIME_H		1
#define HAVE_GETCLOCK		0
#define HAVE_CLOCK_GETTIME      1
#define HAVE_SYS_TIMERS_H	0
#define HAVE_TIME_H		1
#define HAVE_SYS_TIMEB_H	1
#define HAVE_WINDOWS_H		0
#define HAVE_SYS_TIMES_H	1
#define HAVE_WINSOCK_H		0
#define HAVE_LIMITS_H		1
#define HAVE_WCTYPE_H		1
#define HAVE_INTTYPES_H		1
// #define HAVE_STDINT_H		1
#define HAVE_SYS_RESOURCE_H	1
#define HAVE_GETRUSAGE		1
#define HAVE_SYS_SYSCALL_H	1
#define HAVE_SYS_WAIT_H		1
#define HAVE_VFORK_H		0
#define HAVE_SYS_SELECT_H	1
#define HAVE_FTRUNCATE		1

#undef SUPPORT_LONG_LONGS
#define CONST_O_BINARY		0
#define READDIR_ERRNO_EOF	0

#define INLINE	/* to avoid inlining... */
