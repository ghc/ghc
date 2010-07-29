/* We don't use symbol visibility pragmas on FreeBSD, because it causes
   "undefined reference" errors at link-time.  The true reasons are
   unknown, however FreeBSD 8.x includes GCC 4.2.1 in the base system,
   which might be buggy. */
#if __GNUC__ >= 4 && !defined(freebsd_HOST_OS)
#pragma GCC visibility push(hidden)
#endif
