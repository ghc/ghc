/* -----------------------------------------------------------------------------
 * $Id: HsFFI.h,v 1.2 2000/04/06 13:40:15 panne Exp $
 *
 * (c) The GHC Team, 2000
 *
 * A mapping for Haskell types to C types, including the corresponding bounds.
 * Intended to be used in conjuction with the FFI.
 *
 * WARNING: Keep this file and StgTypes.h in synch!
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSFFI_H
#define HSFFI_H

/* get types from GHC's runtime system */
#include "StgTypes.h"

/* get limits for integral types */
#ifdef HAVE_STDINT_H
/* ISO C 99 requires this */
#define __STDC_LIMIT_MACROS
#include <stdint.h>
#else
/* second best guess (e.g. on Solaris) */
#include <limits.h>
#endif

/* if we had no luck, let's do it for ourselves (assuming 64bit long longs) */
#ifndef INT8_MIN
#define INT8_MIN		(-128)
#define INT16_MIN		(-32767-1)
#define INT32_MIN		(-2147483647-1)
#define INT64_MIN		(-9223372036854775807LL-1)
#define INT8_MAX		(127)
#define INT16_MAX		(32767)
#define INT32_MAX		(2147483647)
#define INT64_MAX		(9223372036854775807LL)
#define UINT8_MAX		(255U)
#define UINT16_MAX		(65535U)
#define UINT32_MAX		(4294967295U)
#define UINT64_MAX		(18446744073709551615ULL)
#endif

/* get limits for floating point types */
#include <float.h>

typedef StgChar			HsChar;
typedef StgInt			HsInt;
typedef StgInt8			HsInt8;
typedef StgInt16		HsInt16;
typedef StgInt32		HsInt32;
typedef StgInt64		HsInt64;
typedef StgWord8		HsWord8;
typedef StgWord16		HsWord16;
typedef StgWord32		HsWord32;
typedef StgWord64		HsWord64;
typedef StgFloat		HsFloat;
typedef StgDouble		HsDouble;
typedef StgBool			HsBool;
typedef void*			HsAddr;         /* this should better match StgAddr */
typedef void*			HsForeignObj;   /* ... and this StgForeignPtr       */
typedef void*			HsStablePtr;    /* NOTE: THIS IS CURRENTLY WRONG!!! */

/* this should correspond to the type of StgChar in StgTypes.h */
#define HS_CHAR_MIN		(0)
#define HS_CHAR_MAX		UINT8_MAX

/* this mirrors the distinction of cases in StgTypes.h */
#if   SIZEOF_VOID_P == 8
#define HS_INT_MIN		INT64_MIN
#define HS_INT_MAX		INT64_MAX
#elif SIZEOF_VOID_P == 4
#define HS_INT_MIN		INT32_MIN
#define HS_INT_MAX		INT32_MAX
#else
#error GHC untested on this architecture: sizeof(void *) != 4 or 8
#endif

#define HS_INT8_MIN		INT8_MIN
#define HS_INT8_MAX		INT8_MAX
#define HS_INT16_MIN		INT16_MIN
#define HS_INT16_MAX		INT16_MAX
#define HS_INT32_MIN		INT32_MIN
#define HS_INT32_MAX		INT32_MAX
#define HS_INT64_MIN		INT64_MIN
#define HS_INT64_MAX		INT64_MAX
#define HS_WORD8_MAX		UINT8_MAX
#define HS_WORD16_MAX		UINT16_MAX
#define HS_WORD32_MAX		UINT32_MAX
#define HS_WORD64_MAX		UINT64_MAX

#ifdef FLOATS_AS_DOUBLES

#define HS_FLOAT_RADIX		DBL_RADIX
#define HS_FLOAT_ROUNDS		DBL_ROUNDS
#define HS_FLOAT_EPSILON	DBL_EPSILON
#define HS_FLOAT_DIG		DBL_DIG
#define HS_FLOAT_MANT_DIG	DBL_MANT_DIG
#define HS_FLOAT_MIN		DBL_MIN
#define HS_FLOAT_MIN_EXP	DBL_MIN_EXP
#define HS_FLOAT_MIN_10_EXP	DBL_MIN_10_EXP
#define HS_FLOAT_MAX		DBL_MAX
#define HS_FLOAT_MAX_EXP	DBL_MAX_EXP
#define HS_FLOAT_MAX_10_EXP	DBL_MAX_10_EXP
			   
#else			   
			   
#define HS_FLOAT_RADIX		FLT_RADIX
#define HS_FLOAT_ROUNDS		FLT_ROUNDS
#define HS_FLOAT_EPSILON	FLT_EPSILON
#define HS_FLOAT_DIG		FLT_DIG
#define HS_FLOAT_MANT_DIG	FLT_MANT_DIG
#define HS_FLOAT_MIN		FLT_MIN
#define HS_FLOAT_MIN_EXP	FLT_MIN_EXP
#define HS_FLOAT_MIN_10_EXP	FLT_MIN_10_EXP
#define HS_FLOAT_MAX		FLT_MAX
#define HS_FLOAT_MAX_EXP	FLT_MAX_EXP
#define HS_FLOAT_MAX_10_EXP	FLT_MAX_10_EXP

#endif /* FLOATS_AS_DOUBLES */

#define HS_DOUBLE_RADIX		DBL_RADIX
#define HS_DOUBLE_ROUNDS	DBL_ROUNDS
#define HS_DOUBLE_EPSILON	DBL_EPSILON
#define HS_DOUBLE_DIG		DBL_DIG
#define HS_DOUBLE_MANT_DIG	DBL_MANT_DIG
#define HS_DOUBLE_MIN		DBL_MIN
#define HS_DOUBLE_MIN_EXP	DBL_MIN_EXP
#define HS_DOUBLE_MIN_10_EXP	DBL_MIN_10_EXP
#define HS_DOUBLE_MAX		DBL_MAX
#define HS_DOUBLE_MAX_EXP	DBL_MAX_EXP
#define HS_DOUBLE_MAX_10_EXP	DBL_MAX_10_EXP

#endif /* HSFFI_H */
