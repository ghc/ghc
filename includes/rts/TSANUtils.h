/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006-2019
 *
 * Utilities for annotating "safe" data races for Thread Sanitizer
 * -------------------------------------------------------------------------- */

#if defined(__SANITIZE_THREAD__)
#define TSAN_ENABLED
#elif defined(__has_feature)
#if __has_feature(thread_sanitizer)
#define TSAN_ENABLED
#endif
#endif

#ifdef TSAN_ENABLED
#define TSAN_ANNOTATE_HAPPENS_BEFORE(addr)                  \
  AnnotateHappensBefore(__FILE__, __LINE__, (void*)(addr))
#define TSAN_ANNOTATE_HAPPENS_AFTER(addr)                   \
  AnnotateHappensAfter(__FILE__, __LINE__, (void*)(addr))
#define TSAN_ANNOTATE_BENIGN_RACE(addr,desc)                \
  AnnotateBenignRaceSized(__FILE__, __LINE__, (void*)(addr), sizeof(*addr), desc)
void AnnotateHappensBefore(const char* f, int l, void* addr);
void AnnotateHappensAfter(const char* f, int l, void* addr);
void AnnotateBenignRaceSized(const char *file,
                             int line,
                             const volatile void *mem,
                             long size,
                             const char *description);
#else
#define TSAN_ANNOTATE_HAPPENS_BEFORE(addr)
#define TSAN_ANNOTATE_HAPPENS_AFTER(addr)
#define TSAN_ANNOTATE_BENIGN_RACE(addr,desc)
#endif
