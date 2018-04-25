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
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * NOTE: assumes #include "ghcconfig.h"
 *
 * Works with or without _POSIX_SOURCE.
 *
 * WARNING: Keep this file, MachDeps.h, and HsFFI.h in synch!
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(mingw32_HOST_OS)
/* Inform mingw we want the ISO rather than Windows printf format specifiers. */
#define __USE_MINGW_ANSI_STDIO 1
#endif

/* ISO C 99 says:
 * "C++ implementations should define these macros only when
 * __STDC_LIMIT_MACROS is defined before <stdint.h> is included."
 *
 * So we need to define it for now to compile with C++ compilers.
 * However, C++11 does not require it anymore so we can remove this once we
 * upgrade to requiring C++11 or newer.
 */
#define __STDC_LIMIT_MACROS
#include <inttypes.h>


/*
 * This module should define types *only*, all beginning with "Stg".
 *
 * Specifically:

        StgInt8,  16, 32, 64
        StgWord8, 16, 32, 64
        StgChar, StgFloat, StgDouble

        ***** All the same size (i.e. sizeof(void *)): *****
        StgPtr                  Basic pointer type
        StgWord                 Unit of heap allocation
        StgInt                  Signed version of StgWord
        StgAddr                 Generic address type

        StgBool, StgVoid, StgPtr, StgOffset,
        StgCode, StgStablePtr, StgFunPtr,
        StgUnion.
 */

/*
 * First, platform-dependent definitions of size-specific integers.
 */

typedef int8_t                   StgInt8;
typedef uint8_t                  StgWord8;

#define STG_INT8_MIN             INT8_MIN
#define STG_INT8_MAX             INT8_MAX
#define STG_WORD8_MAX            UINT8_MAX

#define FMT_Word8                PRIu8

typedef int16_t                  StgInt16;
typedef uint16_t                 StgWord16;

#define STG_INT16_MIN            INT16_MIN
#define STG_INT16_MAX            INT16_MAX
#define STG_WORD16_MAX           UINT16_MAX

#define FMT_Word16               PRIu16

typedef int32_t                  StgInt32;
typedef uint32_t                 StgWord32;

#define STG_INT32_MIN            INT32_MIN
#define STG_INT32_MAX            INT32_MAX
#define STG_WORD32_MAX           UINT32_MAX

#define FMT_Word32               PRIu32
#define FMT_HexWord32            PRIx32
#define FMT_Int32                PRId32

typedef int64_t                  StgInt64;
typedef uint64_t                 StgWord64;

#define STG_INT64_MIN            INT64_MIN
#define STG_INT64_MAX            INT64_MAX
#define STG_WORD64_MAX           UINT64_MAX

#define FMT_Word64               PRIu64
#define FMT_HexWord64            PRIx64
#define FMT_Int64                PRId64

typedef struct { StgWord64 h; StgWord64 l; } StgWord128;

typedef struct { StgWord128 h; StgWord128 l; } StgWord256;

typedef struct { StgWord256 h; StgWord256 l; } StgWord512;

/*
 * Stg{Int,Word} are defined such that they have the exact same size as a
 * void pointer.
 */

#if SIZEOF_VOID_P == 8
typedef int64_t            StgInt;
typedef uint64_t           StgWord;

typedef int32_t            StgHalfInt;
typedef uint32_t           StgHalfWord;

#define STG_INT_MIN        INT64_MIN
#define STG_INT_MAX        INT64_MAX
#define STG_WORD_MAX       UINT64_MAX

#define FMT_Word           FMT_Word64
#define FMT_HexWord        FMT_HexWord64
#define FMT_Int            FMT_Int64

#define strToStgWord       strtoull

#elif SIZEOF_VOID_P == 4
typedef int32_t            StgInt;
typedef uint32_t           StgWord;

typedef int16_t            StgHalfInt;
typedef uint16_t           StgHalfWord;

#define STG_INT_MIN        INT32_MIN
#define STG_INT_MAX        INT32_MAX
#define STG_WORD_MAX       UINT32_MAX

#define FMT_Word           FMT_Word32
#define FMT_HexWord        FMT_HexWord32
#define FMT_Int            FMT_Int32

#define strToStgWord       strtoul

#else
#error GHC untested on this architecture: sizeof(void *) != 4 or 8
#endif

#define W_MASK  (sizeof(W_)-1)

/*
 * Other commonly-used STG datatypes.
 */

typedef void*              StgAddr;
typedef StgWord32          StgChar;
typedef int                StgBool;
typedef float              StgFloat;
typedef double             StgDouble;
typedef StgWord*           StgPtr;           /* heap or stack pointer */
typedef StgWord volatile*  StgVolatilePtr;   /* pointer to volatile word   */
typedef StgWord            StgOffset;        /* byte offset within closure */
typedef StgWord8           StgCode;          /* close enough */
typedef void*              StgStablePtr;
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
