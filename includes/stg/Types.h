/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Various C datatypes used in the run-time system.  This is the
 * lowest-level include file, after ghcconfig.h and RtsConfig.h.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * NOTE: assumes #include "ghcconfig.h"
 * 
 * Works with or without _POSIX_SOURCE.
 *
 * WARNING: Keep this file, MachDeps.h, and HsFFI.h in synch!
 *
 * ---------------------------------------------------------------------------*/

#ifndef STGTYPES_H
#define STGTYPES_H

/*
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
	
	StgBool, StgVoid, StgPtr, StgOffset, 
	StgCode, StgStablePtr, StgFunPtr,
	StgUnion.
 */

/*
 * First, platform-dependent definitions of size-specific integers.
 */

typedef signed   char            StgInt8;
typedef unsigned char            StgWord8;

typedef signed   short           StgInt16;
typedef unsigned short           StgWord16;

#if SIZEOF_INT == 4
typedef signed   int             StgInt32;
typedef unsigned int             StgWord32;
#define FMT_Word32    "u"
#define FMT_HexWord32 "x"
#define FMT_Int32     "d"
#elif SIZEOF_LONG == 4
typedef signed   long            StgInt32;
typedef unsigned long            StgWord32;
#define FMT_Word32    "lu"
#define FMT_HexWord32 "lx"
#define FMT_Int32     "ld"
#else
#error GHC untested on this architecture: sizeof(int) != 4
#endif

#if SIZEOF_LONG == 8
typedef signed   long          StgInt64;
typedef unsigned long          StgWord64;
#define FMT_Word64    "lu"
#define FMT_HexWord64 "lx"
#define FMT_Int64     "ld"
#elif SIZEOF_LONG_LONG == 8
typedef signed long long int   StgInt64;
typedef unsigned long long int StgWord64;
#define FMT_Word64    "llu"
#define FMT_HexWord64 "llx"
#define FMT_Int64     "lld"
#else
#error cannot find a way to define StgInt64
#endif

typedef struct { StgWord64 h; StgWord64 l; } StgWord128;

typedef struct { StgWord128 h; StgWord128 l; } StgWord256;

typedef struct { StgWord256 h; StgWord256 l; } StgWord512;

/*
 * Define the standard word size we'll use on this machine: make it
 * big enough to hold a pointer.
 */

#if SIZEOF_VOID_P == 8
typedef StgInt64           StgInt;
typedef StgWord64          StgWord;
typedef StgInt32           StgHalfInt;
typedef StgWord32          StgHalfWord;
#define FMT_Word     FMT_Word64
#define FMT_HexWord  FMT_HexWord64
#define FMT_Int      FMT_Int64
#else
#if SIZEOF_VOID_P == 4
typedef StgInt32           StgInt; 
typedef StgWord32          StgWord;
typedef StgInt16           StgHalfInt;
typedef StgWord16          StgHalfWord;
#define FMT_Word     FMT_Word32
#define FMT_HexWord  FMT_HexWord32
#define FMT_Int      FMT_Int32
#else
#error GHC untested on this architecture: sizeof(void *) != 4 or 8
#endif
#endif

#define W_MASK  (sizeof(W_)-1)

/*
 * Other commonly-used STG datatypes.
 */

typedef void*              StgAddr;
typedef StgWord32          StgChar;
typedef int                StgBool;
typedef float		   StgFloat;
typedef double		   StgDouble;
typedef StgWord*           StgPtr;           /* heap or stack pointer */
typedef StgWord volatile*  StgVolatilePtr;   /* pointer to volatile word   */
typedef StgWord            StgOffset;        /* byte offset within closure */
typedef StgWord8 	   StgCode;  	     /* close enough */
typedef void*		   StgStablePtr;
typedef StgWord8*          StgByteArray;

/*
  Types for generated C functions when compiling via C.

  The C functions take no arguments, and return a pointer to the next
  function to be called use: Ptr to Fun that returns a Ptr to Fun
  which returns Ptr to void

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
