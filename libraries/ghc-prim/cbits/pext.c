#include "Rts.h"
#include "MachDeps.h"

/*
 * The algorithm for PEXT or generalized extract or compress is from
 * Henry S. Warren, Hacker's Delight, 2nd ed, p 153, Figure 7-10
 * "Parallel suffix method for the /compress/ operation".
 */

StgWord64
hs_pext64(StgWord64 src, StgWord64 mask)
{
  uint64_t mk, mp, mv, t;

  src = src & mask;
  mk = ~mask << 1;

  for (int i = 0; i < 6 ; i++) {
    mp = mk ^ (mk << 1);
    mp = mp ^ (mp << 2);
    mp = mp ^ (mp << 4);
    mp = mp ^ (mp << 8);
    mp = mp ^ (mp << 16);
    mp = mp ^ (mp << 32);
    mv = mp & mask;
    mask = (mask ^ mv) | (mv >> (1 << i));
    t = src & mv;
    src = (src ^ t) | (t >> (1 << i));
    mk = mk & ~mp;
  }
  return src;
}

StgWord
hs_pext32(StgWord src, StgWord mask)
{
  uint32_t mk, mp, mv, t;

  src = src & mask;
  mk = ~mask << 1;

  for (int i = 0; i < 5 ; i++) {
    mp = mk ^ (mk << 1);
    mp = mp ^ (mp << 2);
    mp = mp ^ (mp << 4);
    mp = mp ^ (mp << 8);
    mp = mp ^ (mp << 16);
    mv = mp & mask;
    mask = (mask ^ mv) | (mv >> (1 << i));
    t = src & mv;
    src = (src ^ t) | (t >> (1 << i));
    mk = mk & ~mp;
  }
  return src;
}

StgWord
hs_pext16(StgWord src, StgWord mask)
{
  uint16_t mk, mp, mv, t;

  src = src & mask;
  mk = ~mask << 1;

  for (int i = 0; i < 4 ; i++) {
    mp = mk ^ (mk << 1);
    mp = mp ^ (mp << 2);
    mp = mp ^ (mp << 4);
    mp = mp ^ (mp << 8);
    mv = mp & mask;
    mask = (mask ^ mv) | (mv >> (1 << i));
    t = src & mv;
    src = (src ^ t) | (t >> (1 << i));
    mk = mk & ~mp;
  }
  return src;
}

StgWord
hs_pext8(StgWord src, StgWord mask)
{
  uint8_t mk, mp, mv, t;

  src = src & mask;
  mk = ~mask << 1;

  for (int i = 0; i < 3 ; i++) {
    mp = mk ^ (mk << 1);
    mp = mp ^ (mp << 2);
    mp = mp ^ (mp << 4);
    mv = mp & mask;
    mask = (mask ^ mv) | (mv >> (1 << i));
    t = src & mv;
    src = (src ^ t) | (t >> (1 << i));
    mk = mk & ~mp;
  }
  return src;
}
