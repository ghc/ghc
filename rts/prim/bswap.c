#include "Rts.h"

StgWord
hs_bswap16(StgWord x)
{
  x = (StgWord16)x;
  return (StgWord16)((x >> 8) | (x << 8));
}

StgWord
hs_bswap32(StgWord x)
{
  x = (StgWord32)x;
  return (StgWord32)((x >> 24) | ((x >> 8) & 0xff00) |
                     (x << 24) | ((x & 0xff00) << 8));
}

StgWord64
hs_bswap64(StgWord64 x)
{
  return ( (x >> 56)                | (x << 56)
         | ((x >> 40) & 0xff00)     | ((x & 0xff00) << 40)
         | ((x >> 24) & 0xff0000)   | ((x & 0xff0000) << 24)
         | ((x >> 8)  & 0xff000000) | ((x & 0xff000000) << 8)
         );
}
