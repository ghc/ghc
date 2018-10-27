#include "Rts.h"
#include "MachDeps.h"

StgWord64
hs_pdep64(StgWord64 src, StgWord64 mask)
{
  uint64_t m0, mk, mp, mv, t;
  uint64_t array[6];

  m0 = mask;
  mk = ~mask << 1;

  for (int i = 0; i < 6 ; i++) {
    mp = mk ^ (mk << 1);
    mp = mp ^ (mp << 2);
    mp = mp ^ (mp << 4);
    mp = mp ^ (mp << 8);
    mp = mp ^ (mp << 16);
    mp = mp ^ (mp << 32);
    mv = mp & mask;
    array[i] = mv;
    mask = (mask ^ mv) | (mv >> (1 << i));
    mk = mk & ~mp;
  }

  for (int i = 5; i >= 0; i--) {
    mv = array[i];
    t = src << (1 << i);
    src = (src & ~ mv) | (t & mv);
  }
  return src & m0;
}

StgWord
hs_pdep32(StgWord src, StgWord mask)
{
  uint32_t m0, mk, mp, mv, t;
  uint32_t array[5];

  m0 = mask;
  mk = ~mask << 1;

  for (int i = 0; i < 5 ; i++) {
    mp = mk ^ (mk << 1);
    mp = mp ^ (mp << 2);
    mp = mp ^ (mp << 4);
    mp = mp ^ (mp << 8);
    mp = mp ^ (mp << 16);
    mv = mp & mask;
    array[i] = mv;
    mask = (mask ^ mv) | (mv >> (1 << i));
    mk = mk & ~mp;
  }

  for (int i = 4; i >= 0; i--) {
    mv = array[i];
    t = src << (1 << i);
    src = (src & ~ mv) | (t & mv);
  }
  return src & m0;
}

StgWord
hs_pdep16(StgWord src, StgWord mask)
{
  uint16_t m0, mk, mp, mv, t;
  uint16_t array[4];

  m0 = mask;
  mk = ~mask << 1;

  for (int i = 0; i < 4 ; i++) {
    mp = mk ^ (mk << 1);
    mp = mp ^ (mp << 2);
    mp = mp ^ (mp << 4);
    mp = mp ^ (mp << 8);
    mv = mp & mask;
    array[i] = mv;
    mask = (mask ^ mv) | (mv >> (1 << i));
    mk = mk & ~mp;
  }

  for (int i = 3; i >= 0; i--) {
    mv = array[i];
    t = src << (1 << i);
    src = (src & ~ mv) | (t & mv);
  }
  return src & m0;
}

StgWord
hs_pdep8(StgWord src, StgWord mask)
{
  uint8_t m0, mk, mp, mv, t;
  uint8_t array[3];

  m0 = mask;
  mk = ~mask << 1;

  for (int i = 0; i < 3 ; i++) {
    mp = mk ^ (mk << 1);
    mp = mp ^ (mp << 2);
    mp = mp ^ (mp << 4);
    mv = mp & mask;
    array[i] = mv;
    mask = (mask ^ mv) | (mv >> (1 << i));
    mk = mk & ~mp;
  }

  for (int i = 2; i >= 0; i--) {
    mv = array[i];
    t = src << (1 << i);
    src = (src & ~ mv) | (t & mv);
  }
  return src & m0;
}
