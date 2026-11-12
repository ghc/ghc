/* -----------------------------------------------------------------------------
 *
 * (c) 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group
 *
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * Fast memcmp for the GHC RTS.
 *
 * 'hs_memcmp' has memcmp(3) semantics: it compares the first 'n' bytes of
 * the areas starting at 's1' and 's2' as unsigned bytes and returns the
 * difference of the first differing pair, or 0 if the areas compare equal.
 * Unlike ISO C memcmp, 'hs_memcmp(s1, s2, 0)' is well-defined for any
 * pointers (including NULL or dangling ones from empty 'ByteArray#'
 * slices) and returns 0 without dereferencing either pointer.
 *
 * Dispatch:
 *   - On non-glibc Linux on x86_64 or little-endian AArch64, the
 *     hand-written SIMD implementation 'hs_memcmp_simd' (rts/memcmp/) is
 *     used. Its reads are strictly bounded to [0, n), so no read ever
 *     crosses the object bounds.
 *   - On other non-glibc Linux architectures a portable 64-bit
 *     word-at-a-time loop with byte tail is used. Words are loaded with
 *     memcpy so there are no alignment or aliasing issues; only word
 *     equality is tested, so the result is endianness independent and
 *     the exact unsigned byte difference is produced by the tail.
 *   - Everywhere else (glibc, Darwin, Windows, BSD, wasm, ...) the
 *     platform memcmp is used.
 *
 * --------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#if defined(__has_include)
#if __has_include(<features.h>)
#include <features.h>
#endif
#endif

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#if (defined(linux_HOST_OS) || defined(__linux__)) && !defined(__GLIBC__) && \
    !defined(__BIONIC__) && !defined(__ANDROID__) && !defined(__UCLIBC__) && \
    (defined(x86_64_HOST_ARCH) || \
     (defined(aarch64_HOST_ARCH) && !defined(WORDS_BIGENDIAN)) || \
     (defined(__ELF__) && \
      (defined(__x86_64__) || (defined(__aarch64__) && !defined(__AARCH64EB__)))))
#define HS_MEMCMP_HAVE_SIMD 1
extern int hs_memcmp_simd(const void *s1, const void *s2, size_t n);
#endif

/* Prevent GCC from synthesising recursive memcmp calls out of the portable
 * byte-comparison loop when MuslMemcmp.c forwards memcmp -> hs_memcmp. */
#if defined(__GNUC__) && !defined(__clang__)
#define HS_MEMCMP_NO_DISTRIBUTE \
    __attribute__((optimize("no-tree-loop-distribute-patterns")))
#else
#define HS_MEMCMP_NO_DISTRIBUTE
#endif

HS_MEMCMP_NO_DISTRIBUTE
int hs_memcmp(const void *s1, const void *s2, size_t n)
{
#if (defined(linux_HOST_OS) || defined(__linux__)) && !defined(__GLIBC__) && \
    !defined(__BIONIC__) && !defined(__ANDROID__) && !defined(__UCLIBC__)
#if defined(HS_MEMCMP_HAVE_SIMD)
    return hs_memcmp_simd(s1, s2, n);
#else
    /* Portable strictly-bounded fallback for other non-glibc Linux targets
     * (e.g. riscv64, i386, arm, big-endian aarch64). */
    const unsigned char *a = (const unsigned char *)s1;
    const unsigned char *b = (const unsigned char *)s2;

    while (n >= 8)
    {
        uint64_t w1;
        uint64_t w2;
        memcpy(&w1, a, 8);
        memcpy(&w2, b, 8);
        if (w1 != w2)
        {
            break;
        }
        a += 8;
        b += 8;
        n -= 8;
    }

    while (n > 0)
    {
        if (*a != *b)
        {
            return (int)*a - (int)*b;
        }
        a++;
        b++;
        n--;
    }

    return 0;
#endif
#else
    /* n == 0 must not dereference the pointers: empty ByteArray# slices may
     * pass NULL or dangling pointers, for which ISO C memcmp is undefined
     * behaviour (glibc marks its arguments nonnull). */
    if (n == 0)
    {
        return 0;
    }
    return memcmp(s1, s2, n);
#endif
}
