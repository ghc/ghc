#include "Rts.h"
#include <math.h>
#include <fenv.h>

extern StgFloat hs_word2float32(StgWord x);
StgFloat
hs_word2float32(StgWord x)
{
  int r = fegetround();
  fesetround(FE_TOWARDZERO);
  // We need to mark y as "volatile" otherwise GCC may perform all the
  // fegetround/fesetround calls before performing the actual conversion
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
  // "volatile" for the same reason as above
  volatile StgDouble y = (StgDouble)x;
  fesetround(r);
  return y;
}
