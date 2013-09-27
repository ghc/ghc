/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Primitive floating-point operations
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_PRIMFLOAT_H
#define RTS_PRIMFLOAT_H

StgDouble __int_encodeDouble (I_ j, I_ e);
StgFloat  __int_encodeFloat (I_ j, I_ e);
StgDouble __word_encodeDouble (W_ j, I_ e);
StgFloat  __word_encodeFloat (W_ j, I_ e);

#endif /* RTS_PRIMFLOAT_H */
