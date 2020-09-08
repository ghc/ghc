#include "Rts.h"

extern StgFloat hs_word32_to_float32(StgWord32 x);
StgFloat
hs_word32_to_float32(StgWord32 x)
{
  return x;
}

extern StgDouble hs_word32_to_float64(StgWord32 x);
StgDouble
hs_word32_to_float64(StgWord32 x)
{
  return x;
}

extern StgFloat hs_word64_to_float32(StgWord64 x);
StgFloat
hs_word64_to_float32(StgWord64 x)
{
  return x;
}

extern StgDouble hs_word64_to_float64(StgWord64 x);
StgDouble
hs_word64_to_float64(StgWord64 x)
{
  return x;
}
