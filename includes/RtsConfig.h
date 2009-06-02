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

#ifndef RTSCONFIG_H
#define RTSCONFIG_H

/*
 * SUPPORT_LONG_LONGS controls whether we need to support long longs on a
 * particular platform.   On 64-bit platforms, we don't need to support
 * long longs since regular machine words will do just fine.
 */
#if HAVE_LONG_LONG && SIZEOF_VOID_P < 8
#define SUPPORT_LONG_LONGS 1
#endif

/*
 * Whether the runtime system will use libbfd for debugging purposes.
 */
#if defined(DEBUG) && defined(HAVE_BFD_H) && defined(HAVE_LIBBFD) && !defined(_WIN32)
#define USING_LIBBFD 1
#endif

/* -----------------------------------------------------------------------------
   Labels - entry labels & info labels point to the same place in
   TABLES_NEXT_TO_CODE, so we only generate the _info label.  Jumps
   must therefore be directed to foo_info rather than foo_entry when
   TABLES_NEXT_TO_CODE is on.

   This isn't a good place for these macros, but they need to be
   available to .cmm sources as well as C and we don't have a better
   place.
   -------------------------------------------------------------------------- */

#ifdef TABLES_NEXT_TO_CODE
#define ENTRY_LBL(f) f##_info
#else
#define ENTRY_LBL(f) f##_entry
#endif

#ifdef TABLES_NEXT_TO_CODE
#define RET_LBL(f) f##_info
#else
#define RET_LBL(f) f##_ret
#endif

/* -----------------------------------------------------------------------------
   Signals - supported on non-PAR versions of the runtime.  See RtsSignals.h.
   -------------------------------------------------------------------------- */

#define RTS_USER_SIGNALS 1

/* Profile spin locks */

#define PROF_SPIN

#endif /* RTSCONFIG_H */
