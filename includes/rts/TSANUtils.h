/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006-2019
 *
 * Utilities for annotating "safe" data races for Thread Sanitizer
 * -------------------------------------------------------------------------- */

/*
 * Note [ThreadSanitizer]
 * ~~~~~~~~~~~~~~~~~~~~~~~
 * ThreadSanitizer (abbreviated TSAN) is a library and set of compiler
 * instrumentation (supported by both GCC and Clang) for checking C/C++ code
 * for data races.
 *
 * In GHC we use it to check the runtime system implementation (but not yet
 * generated code). TSAN requires that the checked program uses C++11-style
 * atomics for all potentially-racing accesses. Note that we use the __atomic_*
 * builtin operations but not the C11 _Atomic types to maintain compatibility
 * with older compilers.
 *
 * In addition to the atomic operations themselves, TSAN provides a variety of
 * annotation operations which can be used to annotate cases where the
 * intended semantics are either ambiguous or intentionally racy (known as a
 * *benign race*).
 *
 * Finally, there are a few benign races which we can't easily annotate. To
 * silence these errors we have a suppressions file in rts/.tsan-suppressions.
 * In general it's best to add suppressions only as a last resort, when the
 * more precise annotation functions prove to be insufficient.
 *
 * Users guide: https://github.com/google/sanitizers/wiki/ThreadSanitizerCppManual
 */

#if defined(__SANITIZE_THREAD__)
#define TSAN_ENABLED
#elif defined(__has_feature)
#if __has_feature(thread_sanitizer)
#define TSAN_ENABLED
#endif
#endif

#if defined(TSAN_ENABLED)
#if !defined(HAVE_C11_ATOMICS)
#error TSAN cannot be enabled without C11 atomics suppoort.
#endif

#define TSAN_ANNOTATE_HAPPENS_BEFORE(addr)                              \
    AnnotateHappensBefore(__FILE__, __LINE__, (void*)(addr))
#define TSAN_ANNOTATE_HAPPENS_AFTER(addr)                               \
    AnnotateHappensAfter(__FILE__, __LINE__, (void*)(addr))
#define TSAN_ANNOTATE_BENIGN_RACE_SIZED(addr,size,desc)                 \
    AnnotateBenignRaceSized(__FILE__, __LINE__, (void*)(addr), size, desc)
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
#define TSAN_ANNOTATE_BENIGN_RACE_SIZED(addr,size,desc)
#endif

#define TSAN_ANNOTATE_BENIGN_RACE(addr,desc)                            \
    TSAN_ANNOTATE_BENIGN_RACE_SIZED((void*)(addr), sizeof(*addr), desc)
