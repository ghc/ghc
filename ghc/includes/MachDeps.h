/* -----------------------------------------------------------------------------
 * $Id: MachDeps.h,v 1.4 2000/01/25 14:39:14 panne Exp $
 *
 * (c) The GRASP/AQUA Project, Glasgow University, 1998
 * (c) The GHC Team, 1998-1999
 * 
 * Definitions that characterise machine specific properties of basic
 * Stg types provided as unboxed types (mirrors the typedefs in
 * StgTypes.)
 *
 * NB: THIS FILE IS INCLUDED IN HASKELL SOURCE!
 * ---------------------------------------------------------------------------*/

#ifndef MACHDEPS_H
#define MACHDEPS_H

#include "config.h"

#define CHAR_SIZE_IN_BYTES	1
#define ADDR_SIZE_IN_BYTES	SIZEOF_VOID_P
#define INT_SIZE_IN_BYTES	SIZEOF_LONG
#define WORD_SIZE_IN_BYTES	SIZEOF_LONG

#if SIZEOF_DOUBLE == SIZEOF_VOID_P
#define FLOAT_SIZE_IN_BYTES 	SIZEOF_DOUBLE
#define DOUBLE_SIZE_IN_BYTES	SIZEOF_DOUBLE
#else
#define FLOAT_SIZE_IN_BYTES 	SIZEOF_FLOAT
#define DOUBLE_SIZE_IN_BYTES	SIZEOF_DOUBLE
#endif

#define SIZEOF_INT8             SIZEOF_CHAR
#define ALIGNMENT_INT8          ALIGNMENT_CHAR
#define SIZEOF_WORD8            SIZEOF_UNSIGNED_CHAR
#define ALIGNMENT_WORD8         ALIGNMENT_UNSIGNED_CHAR

#define SIZEOF_INT16            SIZEOF_SHORT
#define ALIGNMENT_INT16         ALIGNMENT_SHORT
#define SIZEOF_WORD16           SIZEOF_UNSIGNED_SHORT
#define ALIGNMENT_WORD16        ALIGNMENT_UNSIGNED_SHORT

#if SIZEOF_UNSIGNED_INT == 4
#define SIZEOF_INT32            ALIGNMENT_INT
#define ALIGNMENT_INT32         SIZEOF_INT
#define SIZEOF_WORD32           ALIGNMENT_UNSIGNED_INT
#define ALIGNMENT_WORD32        SIZEOF_UNSIGNED_INT
#else
#error GHC untested on this architecture: sizeof(unsigned int) != 4
#endif

#if HAVE_LONG_LONG && SIZEOF_VOID_P < 8
/* assume long long is 64 bits */
#define SIZEOF_INT64            SIZEOF_LONG_LONG
#define ALIGNMENT_INT64         ALIGNMENT_LONG_LONG
#define SIZEOF_WORD64           SIZEOF_UNSIGNED_LONG_LONG
#define ALIGNMENT_WORD64        ALIGNMENT_UNSIGNED_LONG_LONG
#elif SIZEOF_LONG == 8
#define SIZEOF_INT64            SIZEOF_LONG
#define ALIGNMENT_INT64         ALIGNMENT_LONG
#define SIZEOF_WORD64           SIZEOF_UNSIGNED_LONG
#define ALIGNMENT_WORD64        ALIGNMENT_UNSIGNED_LONG
#else
#error GHC untested on this architecture: sizeof(void *) < 8 and no long longs.
#endif

#endif
