#include "Rts.h"

W_ hs_mulIntMayOflo(W_ a, W_ b) {
  I_ r;
  return (W_)__builtin_mul_overflow((I_)a, (I_)b, &r);
}
