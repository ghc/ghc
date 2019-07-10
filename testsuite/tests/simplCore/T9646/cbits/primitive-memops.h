#if !defined(haskell_primitive_memops_h)
#define haskell_primitive_memops_h

#include <stdlib.h>
#include <stddef.h>
#include <HsFFI.h>

void hsprimitive_memset_Word (HsWord *, ptrdiff_t, size_t, HsWord);

#endif
