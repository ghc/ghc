#include <stdio.h>
#include <immintrin.h>

__m128 mvec   = {1.0, 2.0, 3.0, 42.0};
float  fvec[] = {.0, .0, .0, .0};

/* Uses movaps instruction to move data between XMMn <-> mem. Fails
 * with segfault when data section is not properly aligned (16 byte).
 */
long foo(void)
{
        _mm_store_ps(fvec, mvec);
        return (long) fvec[3];
}
