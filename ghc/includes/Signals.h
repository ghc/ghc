/* -----------------------------------------------------------------------------
 * $Id: Signals.h,v 1.1 2002/09/06 14:34:14 simonmar Exp $
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

extern int stg_sig_install (int, int, StgStablePtr *, void *);

#endif // SIGNALS_H

