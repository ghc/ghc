/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Primitive floating-point operations
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#ifndef RTS_EXPORT
# define RTS_EXPORT
#endif

RTS_EXPORT StgDouble __int_encodeDouble (I_ j, I_ e);
RTS_EXPORT StgFloat  __int_encodeFloat (I_ j, I_ e);
RTS_EXPORT StgDouble __word_encodeDouble (W_ j, I_ e);
RTS_EXPORT StgFloat  __word_encodeFloat (W_ j, I_ e);
