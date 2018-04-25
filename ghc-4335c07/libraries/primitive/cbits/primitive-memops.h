#ifndef haskell_primitive_memops_h
#define haskell_primitive_memops_h

#include <stdlib.h>
#include <stddef.h>
#include <HsFFI.h>

void hsprimitive_memcpy( void *dst, ptrdiff_t doff, void *src, ptrdiff_t soff, size_t len );
void hsprimitive_memmove( void *dst, ptrdiff_t doff, void *src, ptrdiff_t soff, size_t len );
int  hsprimitive_memcmp( HsWord8 *s1, HsWord8 *s2, size_t n );

void hsprimitive_memset_Word8 (HsWord8 *, ptrdiff_t, size_t, HsWord);
void hsprimitive_memset_Word16 (HsWord16 *, ptrdiff_t, size_t, HsWord);
void hsprimitive_memset_Word32 (HsWord32 *, ptrdiff_t, size_t, HsWord);
void hsprimitive_memset_Word64 (HsWord64 *, ptrdiff_t, size_t, HsWord64);
void hsprimitive_memset_Word (HsWord *, ptrdiff_t, size_t, HsWord);
void hsprimitive_memset_Ptr (HsPtr *, ptrdiff_t, size_t, HsPtr);
void hsprimitive_memset_Float (HsFloat *, ptrdiff_t, size_t, HsFloat);
void hsprimitive_memset_Double (HsDouble *, ptrdiff_t, size_t, HsDouble);
void hsprimitive_memset_Char (HsChar *, ptrdiff_t, size_t, HsChar);

#endif

