/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Various C datatypes used in the run-time system.  This is the
 * lowest-level include file, after ghcconfig.h and RtsConfig.h.
 *
 * This module should define types *only*, all beginning with "Stg".
 *
 * Specifically:

	StgInt8,  16, 32, 64
	StgWord8, 16, 32, 64
	StgChar, StgFloat, StgDouble

	***** All the same size (i.e. sizeof(void *)): *****
	StgPtr			Basic pointer type
	StgWord			Unit of heap allocation
	StgInt			Signed version of StgWord
	StgAddr			Generic address type
	
	StgBool, StgVoid, StgClosurePtr, StgPtr, StgOffset, 
	StgTSOPtr, StgForeignPtr, StgStackOffset, StgStackPtr,
	StgCode, StgArray, StgByteArray, StgStablePtr, StgFunPtr,
	StgUnion.

 * WARNING: Keep this file, MachDeps.h, and HsFFI.h in synch!
 *
 * NOTE: assumes #include "ghcconfig.h"
 * 
 * Works with or without _POSIX_SOURCE.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGTYPES_H
#define STGTYPES_H

/*
 * First, platform-dependent definitions of size-specific integers.
 * Assume for now that the int type is 32 bits.
 * NOTE: Synch the following definitions with MachDeps.h!
 * ToDo: move these into a platform-dependent file.
 */

typedef signed   char            StgInt8;
typedef unsigned char            StgWord8;

typedef signed   short           StgInt16;
typedef unsigned short           StgWord16;

#if SIZEOF_UNSIGNED_INT == 4
typedef signed   int             StgInt32;
typedef unsigned int             StgWord32;
#else
#error GHC untested on this architecture: sizeof(unsigned int) != 4
#endif

#ifdef SUPPORT_LONG_LONGS
/* assume long long is 64 bits */
# ifndef _MSC_VER
typedef signed long long int   StgInt64;
typedef unsigned long long int StgWord64;
# else
typedef __int64 StgInt64;
typedef unsigned __int64 StgWord64;
# endif
#elif SIZEOF_LONG == 8
typedef signed   long          StgInt64;
typedef unsigned long          StgWord64;
#elif defined(__MSVC__)
typedef __int64                StgInt64;
typedef unsigned __int64       StgWord64;
#else
#error GHC untested on this architecture: sizeof(void *) < 8 and no long longs.
#endif

/*
 * Define the standard word size we'll use on this machine: make it
 * big enough to hold a pointer.
 */

#if SIZEOF_VOID_P == 8
typedef StgInt64           StgInt;
typedef StgWord64          StgWord;
typedef StgInt32           StgHalfInt;
typedef StgWord32          StgHalfWord;
#else
#if SIZEOF_VOID_P == 4
typedef StgInt32           StgInt; 
typedef StgWord32          StgWord;
typedef StgInt16           StgHalfInt;
typedef StgWord16          StgHalfWord;
#else
#error GHC untested on this architecture: sizeof(void *) != 4 or 8
#endif
#endif

#define W_MASK  (sizeof(W_)-1)

typedef void*              StgAddr;

/*
 * Other commonly-used STG datatypes.
 */

typedef StgWord32          StgChar;
typedef int                StgBool;

typedef float		   StgFloat;
typedef double		   StgDouble;
                           
typedef void               StgVoid;
                           
typedef struct StgClosure_ StgClosure;
typedef StgClosure*        StgClosurePtr;
typedef StgWord*           StgPtr;           /* pointer into closure       */
typedef StgWord            StgOffset;        /* byte offset within closure */
                           
typedef struct StgTSO_*    StgTSOPtr;

typedef void*              StgForeignPtr;

typedef StgInt             StgStackOffset;   /* offset in words! */

typedef StgWord*           StgStackPtr;

typedef StgWord8 	   StgCode;  	    /* close enough */

typedef StgPtr*            StgArray;        /* the goods of an Array# */
typedef char*		   StgByteArray;    /* the goods of a ByteArray# */

typedef void*		   StgStablePtr;

/*
  Types for the generated C functions
  take no arguments
  return a pointer to the next function to be called
  use: Ptr to Fun that returns a Ptr to Fun which returns Ptr to void

  Note: Neither StgFunPtr not StgFun is quite right (that is, 
  StgFunPtr != StgFun*).  So, the functions we define all have type
  StgFun but we always have to cast them to StgFunPtr when we assign
  them to something.
  The only way round this would be to write a recursive type but
  C only allows that if you're defining a struct or union.
*/

typedef void  *(*(*StgFunPtr)(void))(void);
typedef StgFunPtr StgFun(void);

#endif /* STGTYPES_H */
