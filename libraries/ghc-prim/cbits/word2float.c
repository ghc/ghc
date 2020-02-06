#include "Rts.h"
#include <math.h>
#include <fenv.h>

extern StgFloat hs_word2float32(StgWord x);
StgFloat
hs_word2float32(StgWord x)
{
  int r = fegetround();
  fesetround(FE_TOWARDZERO);
  volatile StgFloat y = (StgFloat)x;
  fesetround(r);
  return y;
}

extern StgDouble hs_word2float64(StgWord x);
StgDouble
hs_word2float64(StgWord x)
{
  int r = fegetround();
  fesetround(FE_TOWARDZERO);
  volatile StgDouble y = (StgDouble)x;
  fesetround(r);
  return y;
}
