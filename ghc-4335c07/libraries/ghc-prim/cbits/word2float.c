#include "Rts.h"

extern StgFloat hs_word2float32(StgWord x);
StgFloat
hs_word2float32(StgWord x)
{
  return x;
}

extern StgDouble hs_word2float64(StgWord x);
StgDouble
hs_word2float64(StgWord x)
{
  return x;
}
