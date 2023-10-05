#include "Rts.h"

extern StgWord16 hs_bswap16(StgWord16 x);
StgWord16
hs_bswap16(StgWord16 x)
{
  return ((x >> 8) | (x << 8));
}

extern StgWord32 hs_bswap32(StgWord32 x);
StgWord32
hs_bswap32(StgWord32 x)
{
  return ((x >> 24) | ((x >> 8) & 0xff00) |
          (x << 24) | ((x & 0xff00) << 8));
}

extern StgWord64 hs_bswap64(StgWord64 x);
StgWord64
hs_bswap64(StgWord64 x)
{
  return ( (x >> 56)                | (x << 56)
         | ((x >> 40) & 0xff00)     | ((x & 0xff00) << 40)
         | ((x >> 24) & 0xff0000)   | ((x & 0xff0000) << 24)
         | ((x >> 8)  & 0xff000000) | ((x & 0xff000000) << 8)
         );
}
