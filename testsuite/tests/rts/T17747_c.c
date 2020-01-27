#include "Rts.h"
#include "sm/HeapAlloc.h"

int malloc_count = 0;

void *mallocByteArray (StgWord len)
{
  malloc_count++;

  /* allocate enough space for the object header + length */
  return malloc(sizeof(StgArrBytes) + len);
}

void freeByteArray (void *arr)
{
    free(arr);
    malloc_count--;
}

int getMallocByteArrayCount(void)
{
  return malloc_count;
}

int c_HEAP_ALLOCED(void * p)
{
  return HEAP_ALLOCED(p);
}

