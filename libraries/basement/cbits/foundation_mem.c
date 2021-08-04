#include <string.h>
#include <stdint.h>
#include "foundation_prim.h"

int _foundation_memcmp(const void *s1, FsOffset off1, const void *s2, FsOffset off2, FsCountOf n)
{
	return memcmp(s1 + off1, s2 + off2, n);
}

FsOffset _foundation_mem_findbyte(uint8_t * const arr, FsOffset startofs, FsOffset endofs, uint8_t ty)
{
    uint8_t *r = memchr(arr + startofs, ty, endofs - startofs);
    return ((r == NULL) ? endofs : r - arr);
}
