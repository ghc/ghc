/* -----------------------------------------------------------------------------
 * $Id: Rts.h,v 1.12 2000/01/13 14:34:01 hwloidl Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Top-level include file for the RTS itself
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_H
#define RTS_H

#ifndef IN_STG_CODE
#define IN_STG_CODE 0
#endif
#include "Stg.h"

/* -----------------------------------------------------------------------------
   RTS Exit codes
   -------------------------------------------------------------------------- */

#if ! defined(EXIT_SUCCESS) || ! defined(EXIT_FAILURE)
/* "stdlib.h" should have defined these; but at least
   on SunOS 4.1.3, this is not so.
*/
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#endif

/* 255 is allegedly used by dynamic linkers to report linking failure */
#define EXIT_INTERNAL_ERROR 254
#define EXIT_DEADLOCK       253
#define EXIT_INTERRUPTED    252
#define EXIT_HEAPOVERFLOW   251
#define EXIT_KILLED         250

/* -----------------------------------------------------------------------------
   Miscellaneous garbage
   -------------------------------------------------------------------------- */

/* declarations for runtime flags/values */
#define MAX_RTS_ARGS 32

/* -----------------------------------------------------------------------------
   Assertions and Debuggery
   -------------------------------------------------------------------------- */

#define IF_RTSFLAGS(c,s)  if (RtsFlags.c) { s; }

/* -----------------------------------------------------------------------------
   Assertions and Debuggery
   -------------------------------------------------------------------------- */

#ifdef DEBUG
#define IF_DEBUG(c,s)  if (RtsFlags.DebugFlags.c) { s; }
#else
#define IF_DEBUG(c,s)  doNothing()
#endif

#if defined(GRAN) && defined(DEBUG)
#define IF_GRAN_DEBUG(c,s)  if (RtsFlags.GranFlags.Debug.c) { s; }
#else
#define IF_GRAN_DEBUG(c,s)  doNothing()
#endif

#if defined(PAR) && defined(DEBUG)
#define IF_PAR_DEBUG(c,s)  if (RtsFlags.ParFlags.Debug.c) { s; }
#else
#define IF_PAR_DEBUG(c,s)  doNothing()
#endif

/* -----------------------------------------------------------------------------
   Attributes
   -------------------------------------------------------------------------- */

#ifdef __GNUC__     /* Avoid spurious warnings                             */
#if __GNUC__ >= 2 && __GNUC_MINOR__ >= 7
#define STG_NORETURN  __attribute__ ((noreturn))
#define STG_UNUSED    __attribute__ ((unused))
#else
#define STG_NORETURN  
#define STG_UNUSED
#endif
#else
#define STG_NORETURN  
#define STG_UNUSED
#endif

/* -----------------------------------------------------------------------------
   Useful macros and inline functions
   -------------------------------------------------------------------------- */

/* 
 * Use this on the RHS of macros which expand to nothing
 * to make sure that the macro can be used in a context which
 * demands a non-empty statement.
 */

#define doNothing() do { } while (0)

#define stg_min(a,b) ({typeof(a) _a = (a), _b = (b); _a <= _b ? _a : _b; })
#define stg_max(a,b) ({typeof(a) _a = (a), _b = (b); _a <= _b ? _b : _a; })

#endif RTS_H
