#include "Rts.h"

/*
Note [Bit reversal primop]
~~~~~~~~~~~~~~~~~~~~~~~~~~

There are two main ways of reversing the bit order of a word: bit twiddling
and using a lookup table.
See [this excellent](https://stackoverflow.com/questions/746171/most-efficient-algorithm-for-bit-reversal-from-msb-lsb-to-lsb-msb-in-c this)
Stack Overflow answer about bit order reversal for (much) more detail.
(Link valid as of March 2019.)

To summarize,

    * the lookup table is faster, but much more memory-heavy e.g.
      doing it for 64-bit words can take 64KB if only 16-bits are reversed at
      a time.
    * working directly with bits is slower (roughly on the same order of
      magnitude as the previous alternative), but uses much less memory as
      bit-wise operators aren't space-onerous.

The code below uses the latter option. If in the future the performance of this
primop must be improved, the information provided in this comment should be
useful in making the decision of which changes to make.

For more information on how the below bit-twiddling functions came to be, see
[this](http://graphics.stanford.edu/~seander/bithacks.html#ReverseParallel)
page.
*/

extern StgWord hs_bitrev8(StgWord x);
StgWord
hs_bitrev8(StgWord x)
{
  x = ((x >> 1) & 0x55) | ((x & 0x55) << 1 );
  x = ((x >> 2) & 0x33) | ((x & 0x33) << 2 );
  x = ((x >> 4) & 0x0F) | ((x & 0x0F) << 4 );
  return x;
}

extern StgWord16 hs_bitrev16(StgWord16 x);
StgWord16
hs_bitrev16(StgWord16 x)
{
  x = ((x >> 1) & 0x5555) | ((x & 0x5555) << 1 );
  x = ((x >> 2) & 0x3333) | ((x & 0x3333) << 2 );
  x = ((x >> 4) & 0x0F0F) | ((x & 0x0F0F) << 4 );
  x = ((x >> 8) & 0x00FF) | ((x & 0x00FF) << 8 );
  return x;
}

extern StgWord32 hs_bitrev32(StgWord32 x);
StgWord32
hs_bitrev32(StgWord32 x)
{
  x = ((x >> 1) & 0x55555555) | ((x & 0x55555555) << 1 );
  x = ((x >> 2) & 0x33333333) | ((x & 0x33333333) << 2 );
  x = ((x >> 4) & 0x0F0F0F0F) | ((x & 0x0F0F0F0F) << 4 );
  x = ((x >> 8) & 0x00FF00FF) | ((x & 0x00FF00FF) << 8 );
  x = ( x >> 16             ) | ( x << 16              );
  return x;
}

extern StgWord64 hs_bitrev64(StgWord64 x);
StgWord64
hs_bitrev64(StgWord64 x)
{
  // swap odd and even bits
  x = ((x >> 1)  & 0x5555555555555555) | ((x & 0x5555555555555555) << 1 );
  // swap consecutive pairs of bits
  x = ((x >> 2)  & 0x3333333333333333) | ((x & 0x3333333333333333) << 2 );
  // swap consecutive pairs of nibbles (a nibble is 4 bits)
  x = ((x >> 4)  & 0x0F0F0F0F0F0F0F0F) | ((x & 0x0F0F0F0F0F0F0F0F) << 4 );
  // swap consecutive pairs of bytes
  x = ((x >> 8)  & 0x00FF00FF00FF00FF) | ((x & 0x00FF00FF00FF00FF) << 8 );
  // swap consecutive pairs of 16-bit words
  x = ((x >> 16) & 0x0000FFFF0000FFFF) | ((x & 0x0000FFFF0000FFFF) << 16);
  // swap 32-bit long pairs
  x = ( x >> 32                      ) | ( x << 32                      );
  return x;
}