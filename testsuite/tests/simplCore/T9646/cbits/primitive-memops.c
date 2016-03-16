#include <string.h>
#include "primitive-memops.h"

void hsprimitive_memset_Word (HsWord *p, ptrdiff_t off, size_t n, HsWord x)
{
  p += off;
  if (x == 0)
    memset(p, 0, n * sizeof(HsWord));
  else if (sizeof(HsWord) == sizeof(int)*2) {
    int *q = (int *)p;
    const int *r = (const int *)(void *)&x;
    while (n>0) {
      q[0] = r[0];
      q[1] = r[1];
      q += 2;
      --n;
    }
  }
  else {
    while (n>0) {
      *p++ = x;
      --n;
    }
  }
}
