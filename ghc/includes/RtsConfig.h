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
#if defined(DEBUG) && defined(HAVE_BFD_H) && !defined(_WIN32) && !defined(PAR) && !defined(GRAN)
#define USING_LIBBFD 1
#endif

/* Turn lazy blackholing and eager blackholing on/off.
 *
 * Using eager blackholing makes things easier to debug because
 * the blackholes are more predictable - but it's slower and less sexy.
 *
 * For now, do lazy and not eager.
 */

/* TICKY_TICKY needs EAGER_BLACKHOLING to verify no double-entries of
 * single-entry thunks.
 */
/* #if defined(TICKY_TICKY) || defined(SMP) */
#if defined(TICKY_TICKY)
#  define EAGER_BLACKHOLING
#else
#  define LAZY_BLACKHOLING
#endif

/* TABLES_NEXT_TO_CODE says whether to assume that info tables are
 * assumed to reside just before the code for a function.
 *
 * UNDEFINING THIS WON'T WORK ON ITS OWN.  You have been warned.
 */
#if !defined(USE_MINIINTERPRETER) && !defined(ia64_HOST_ARCH) && !defined (powerpc64_HOST_ARCH)
#define TABLES_NEXT_TO_CODE
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

#if !defined(PAR)
#define RTS_USER_SIGNALS 1
#endif

#endif /* RTSCONFIG_H */
