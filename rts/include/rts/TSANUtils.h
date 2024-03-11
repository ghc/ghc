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
 * GHC can be configured with two extents of TSAN instrumentation:
 *
 *  - instrumenting the C RTS (by passing `-optc-fsanitize=thread`
 *    when compiling the RTS)
 *
 *  - instrumenting both the C RTS and Cmm memory accesses (by passing
 *    `-optc-fsanitize=thread -fcmm-thread-sanitizer` to all GHC invocations).
 *
 * These two modes can be realized in Hadrian using the `+thread_sanitizer`
 * and `+thread_sanitizer_cmm` flavour transformers.
 *
 * Tips and tricks:
 *
 *  - One should generally run TSAN instrumented programs with the environment
 *    variable
 *
 *      TSAN_OPTIONS=suppressions=$ghc_root/rts/.tsan-suppressions
 *
 *    to maximize signal-to-noise.
 *
 *  - One can set a breakpoint on `__tsan_on_report` in a debugger to pause when
 *    a TSAN report is found.
 *
 *  - TSAN-instrumented will by default exit with code 66 when a violation has
 *    been found. However, this can be disabled by setting
 *    `TSAN_OPTIONS=exitcode=0`
 *
 *  - If TSAN is able to report useful stack traces it may help to set
 *    `TSAN_OPTIONS=history_size=3` or greater (up to 7). This increases the
 *    size of TSAN's per-thread memory access history buffer.
 *
 * - TSAN report messages can be redirected to a file using
 *   `TSAN_OPTIONS=log_path=...`
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

#if !defined(CMINUSMINUS)
#if defined(TSAN_ENABLED)

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

#if defined(TSAN_ENABLED) && defined(__clang__)
#include <sanitizer/tsan_interface_atomic.h>
#else
typedef char __tsan_atomic8;
typedef short __tsan_atomic16;
typedef int __tsan_atomic32;
typedef long __tsan_atomic64;
#endif

__tsan_atomic64 ghc_tsan_atomic64_compare_exchange(volatile __tsan_atomic64 *ptr, __tsan_atomic64 expected, __tsan_atomic64 new_value, int success_memorder, int failure_memorder);
__tsan_atomic32 ghc_tsan_atomic32_compare_exchange(volatile __tsan_atomic32 *ptr, __tsan_atomic32 expected, __tsan_atomic32 new_value, int success_memorder, int failure_memorder);
__tsan_atomic16 ghc_tsan_atomic16_compare_exchange(volatile __tsan_atomic16 *ptr, __tsan_atomic16 expected, __tsan_atomic16 new_value, int success_memorder, int failure_memorder);
__tsan_atomic8 ghc_tsan_atomic8_compare_exchange(volatile __tsan_atomic8 *ptr, __tsan_atomic8 expected, __tsan_atomic8 new_value, int success_memorder, int failure_memorder);

#endif
