/* -----------------------------------------------------------------------------
 * $Id: Derived.h,v 1.1 2001/07/26 03:08:39 ken Exp $
 *
 * (c) The GHC Team, 1998-2001
 *
 * Configuration information derived from config.h.
 *
 * NOTE: assumes #include "config.h"
 * 
 * NB: THIS FILE IS INCLUDED IN NON-C CODE AND DATA!  #defines only please.
 * ---------------------------------------------------------------------------*/

#ifndef DERIVED_H
#define DERIVED_H

/*
 * SUPPORT_LONG_LONGS controls whether we need to support long longs on a
 * particular platform.   On 64-bit platforms, we don't need to support
 * long longs since regular machine words will do just fine.
 */
#if HAVE_LONG_LONG && SIZEOF_VOID_P < 8
#define SUPPORT_LONG_LONGS 1
#endif

#endif /* DERIVED_H */
