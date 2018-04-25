/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Primitive floating-point operations
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

StgDouble __int_encodeDouble (I_ j, I_ e);
StgFloat  __int_encodeFloat (I_ j, I_ e);
StgDouble __word_encodeDouble (W_ j, I_ e);
StgFloat  __word_encodeFloat (W_ j, I_ e);
