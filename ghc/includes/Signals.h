/* -----------------------------------------------------------------------------
 * $Id: Signals.h,v 1.2 2002/12/05 14:20:55 stolz Exp $
 *
 * (c) The GHC Team, 1998-2002
 *
 * RTS signal handling 
 *
 * ---------------------------------------------------------------------------*/

#ifndef SIGNALS_H
#define SIGNALS_H

#define STG_SIG_DFL  (-1)
#define STG_SIG_IGN  (-2)
#define STG_SIG_ERR  (-3)
#define STG_SIG_HAN  (-4)
#define STG_SIG_RST  (-5)

extern int stg_sig_install (int, int, StgStablePtr *, void *);

#endif // SIGNALS_H

