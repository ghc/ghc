/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * RTS signal handling 
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
