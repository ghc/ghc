#include <x86intrin.h>

__m512i minInt64X8(__m512i a, __m512i b)
{
    return _mm512_min_epi64(a, b);
}
