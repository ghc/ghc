/* -----------------------------------------------------------------------------
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

#if defined(mingw32_HOST_OS)
extern int stg_InstallConsoleEvent(int action, StgStablePtr *handler);
#else
extern int stg_sig_install (int, int, StgStablePtr *, void *);
#endif

#endif // SIGNALS_H

