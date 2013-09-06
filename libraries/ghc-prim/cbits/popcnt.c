#include "Rts.h"

static const unsigned char popcount_tab[] =
{
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8,
};

extern StgWord hs_popcnt8(StgWord x);
StgWord
hs_popcnt8(StgWord x)
{
  return popcount_tab[(unsigned char)x];
}

extern StgWord hs_popcnt16(StgWord x);
StgWord
hs_popcnt16(StgWord x)
{
  return popcount_tab[(unsigned char)x] +
      popcount_tab[(unsigned char)(x >> 8)];
}

extern StgWord hs_popcnt32(StgWord x);
StgWord
hs_popcnt32(StgWord x)
{
  return popcount_tab[(unsigned char)x] +
      popcount_tab[(unsigned char)(x >> 8)] +
      popcount_tab[(unsigned char)(x >> 16)] +
      popcount_tab[(unsigned char)(x >> 24)];
}

extern StgWord hs_popcnt64(StgWord64 x);
StgWord
hs_popcnt64(StgWord64 x)
{
  return popcount_tab[(unsigned char)x] +
      popcount_tab[(unsigned char)(x >> 8)] +
      popcount_tab[(unsigned char)(x >> 16)] +
      popcount_tab[(unsigned char)(x >> 24)] +
      popcount_tab[(unsigned char)(x >> 32)] +
      popcount_tab[(unsigned char)(x >> 40)] +
      popcount_tab[(unsigned char)(x >> 48)] +
      popcount_tab[(unsigned char)(x >> 56)];
}

#ifdef i386_HOST_ARCH

extern StgWord hs_popcnt(StgWord x);
StgWord
hs_popcnt(StgWord x)
{
  return popcount_tab[(unsigned char)x] +
      popcount_tab[(unsigned char)(x >> 8)] +
      popcount_tab[(unsigned char)(x >> 16)] +
      popcount_tab[(unsigned char)(x >> 24)];
}

#else

extern StgWord hs_popcnt(StgWord x);
StgWord
hs_popcnt(StgWord x)
{
  return popcount_tab[(unsigned char)x] +
      popcount_tab[(unsigned char)(x >> 8)] +
      popcount_tab[(unsigned char)(x >> 16)] +
      popcount_tab[(unsigned char)(x >> 24)] +
      popcount_tab[(unsigned char)(x >> 32)] +
      popcount_tab[(unsigned char)(x >> 40)] +
      popcount_tab[(unsigned char)(x >> 48)] +
      popcount_tab[(unsigned char)(x >> 56)];
}

#endif
