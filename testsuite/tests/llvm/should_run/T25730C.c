#include <emmintrin.h>
#include <stdio.h>

__m128i packsi32(__m128i a, __m128i b)
{
    return _mm_packs_epi32(a, b);
}
