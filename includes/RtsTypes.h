/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2008
 *
 * RTS-specific types.
 *
 * ---------------------------------------------------------------------------*/

/* -------------------------------------------------------------------------
   Generally useful typedefs
   ------------------------------------------------------------------------- */

#ifndef RTS_TYPES_H
#define RTS_TYPES_H

typedef unsigned int  nat;           /* at least 32 bits (like int) */
typedef unsigned long lnat;          /* at least 32 bits            */
#ifndef _MSC_VER
typedef unsigned long long ullong;   /* at least 32 bits            */
typedef long long llong;
#else
typedef unsigned __int64   ullong;   /* at least 32 bits            */
typedef __int64 llong;
#endif

/* ullong (64|128-bit) type: only include if needed (not ANSI) */
#if defined(__GNUC__) 
#define LL(x) (x##LL)
#else
#define LL(x) (x##L)
#endif
  
typedef enum { 
    rtsFalse = 0, 
    rtsTrue 
} rtsBool;

/* 
   Types specific to the parallel runtime system.
*/

typedef ullong        rtsTime;

#endif /* RTS_TYPES_H */
