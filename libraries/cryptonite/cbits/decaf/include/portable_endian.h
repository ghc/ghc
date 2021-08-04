/* portable_endian.h not used */

#if defined(__MINGW32__)
// does not exist on MinGW, but unused anyway
extern int posix_memalign(void **, size_t, size_t);
#endif
