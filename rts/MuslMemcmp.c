/* -----------------------------------------------------------------------------
 *
 * (c) 2026 Moritz Angermann <moritz.angermann@iohk.io>, Input Output Group
 *
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * Static-archive memcmp interposer for non-glibc Linux.
 *
 * When the RTS is built for static ways (no -DDYNAMIC) on non-glibc Linux,
 * libHSrts*.a includes this archive member defining a strong 'memcmp' that
 * forwards to 'hs_memcmp'. Because GHC places -lHSrts after Haskell package
 * archives and before -lc, any still-undefined 'memcmp' reference from
 * third-party Haskell/C packages extracts this member before libc.a(memcmp.lo)
 * is scanned. Dynamic RTS shared objects (-DDYNAMIC) compile an empty TU so
 * they never interpose libc's dynamic 'memcmp'.
 *
 * Both 'hs_memcmp' (where the portable C fallback loop lives) and this
 * forwarder carry no-tree-loop-distribute-patterns on GCC so loop distribution
 * cannot turn the fallback back into a recursive memcmp call.
 *
 * --------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include <stddef.h>
#include <string.h>

#if defined(__linux__) && !defined(__GLIBC__) && !defined(__BIONIC__) && \
    !defined(__ANDROID__) && !defined(__UCLIBC__) && !defined(DYNAMIC) && \
    !defined(SHARED)

#if defined(__GNUC__) && !defined(__clang__)
#define HS_MEMCMP_NO_DISTRIBUTE \
    __attribute__((optimize("no-tree-loop-distribute-patterns")))
#else
#define HS_MEMCMP_NO_DISTRIBUTE
#endif

HS_MEMCMP_NO_DISTRIBUTE
int memcmp(const void *s1, const void *s2, size_t n)
{
    return hs_memcmp(s1, s2, n);
}

#else

typedef int hs_musl_memcmp_unused_tu;

#endif
