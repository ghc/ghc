/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * RTS signal handling 
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_SIGNALS_H
#define RTS_SIGNALS_H

/* NB. #included in Haskell code, no prototypes in here. */

/* arguments to stg_sig_install() */
#define STG_SIG_DFL   (-1)
#define STG_SIG_IGN   (-2)
#define STG_SIG_ERR   (-3)
#define STG_SIG_HAN   (-4)
#define STG_SIG_RST   (-5)

#endif /* RTS_SIGNALS_H */
