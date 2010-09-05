/* We don't use symbol visibility pragmas on FreeBSD, because it causes
   "undefined reference" errors at link-time.  The true reasons are
   unknown, however FreeBSD 8.x includes GCC 4.2.1 in the base system,
   which might be buggy. */
/* On Windows, with gcc 4.5.0-1, using visibility hidden gives:
       error: visibility attribute not supported in this configuration; ignored
   */
#if __GNUC__ >= 4 && !defined(freebsd_HOST_OS) && !defined(mingw32_HOST_OS)
#pragma GCC visibility push(hidden)
#endif
