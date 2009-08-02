/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Rts settings.
 *
 * NOTE: assumes #include "ghcconfig.h"
 * 
 * NB: THIS FILE IS INCLUDED IN NON-C CODE AND DATA!  #defines only please.
 * ---------------------------------------------------------------------------*/

#ifndef RTS_CONFIG_H
#define RTS_CONFIG_H

#if defined(TICKY_TICKY) && defined(THREADED_RTS)
#error TICKY_TICKY is incompatible with THREADED_RTS
#endif

/*
 * Whether the runtime system will use libbfd for debugging purposes.
 */
#if defined(DEBUG) && defined(HAVE_BFD_H) && defined(HAVE_LIBBFD) && !defined(_WIN32)
#define USING_LIBBFD 1
#endif

/* -----------------------------------------------------------------------------
   Signals - supported on non-PAR versions of the runtime.  See RtsSignals.h.
   -------------------------------------------------------------------------- */

#define RTS_USER_SIGNALS 1

/* Profile spin locks */

#define PROF_SPIN

#endif /* RTS_CONFIG_H */
