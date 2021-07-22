/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2002
 *
 * Definitions that characterise machine specific properties of basic
 * types (C & Haskell) of a target platform.
 *
 * NB: Keep in sync with HsFFI.h and StgTypes.h.
 * NB: THIS FILE IS INCLUDED IN HASKELL SOURCE!
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

/* Don't allow stage1 (cross-)compiler embed assumptions about target
 * platform. When ghc-stage1 is being built by ghc-stage0 is should not
 * refer to target defines. A few past examples:
 *  - https://gitlab.haskell.org/ghc/ghc/issues/13491
 *  - https://phabricator.haskell.org/D3122
 *  - https://phabricator.haskell.org/D3405
 *
 * In those cases code change assumed target defines like SIZEOF_HSINT
 * are applied to host platform, not target platform.
 *
 * So what should be used instead in GHC_STAGE=1?
 *
 * To get host's equivalent of SIZEOF_HSINT you can use Bits instances:
 *    Data.Bits.finiteBitSize (0 :: Int)
 *
 * To get target's values it is preferred to use runtime target
 * configuration from 'targetPlatform :: DynFlags -> Platform'
 * record.
 *
 * Hence we hide these macros from GHC_STAGE=1
 */

/* Sizes of C types come from here... */
#include "ghcautoconf.h"

/* Sizes of Haskell types follow.  These sizes correspond to:
 *   - the number of bytes in the primitive type (eg. Int#)
 *   - the number of bytes in the external representation (eg. HsInt)
 *   - the scale offset used by writeFooOffAddr#
 *
 * In the heap, the type may take up more space: eg. SIZEOF_INT8 == 1,
 * but it takes up SIZEOF_HSWORD (4 or 8) bytes in the heap.
 */

#define SIZEOF_HSCHAR           SIZEOF_WORD32
#define ALIGNMENT_HSCHAR        ALIGNMENT_WORD32

#define SIZEOF_HSINT            SIZEOF_VOID_P
#define ALIGNMENT_HSINT         ALIGNMENT_VOID_P

#define SIZEOF_HSWORD           SIZEOF_VOID_P
#define ALIGNMENT_HSWORD        ALIGNMENT_VOID_P

#define SIZEOF_HSDOUBLE         SIZEOF_DOUBLE
#define ALIGNMENT_HSDOUBLE      ALIGNMENT_DOUBLE

#define SIZEOF_HSFLOAT          SIZEOF_FLOAT
#define ALIGNMENT_HSFLOAT       ALIGNMENT_FLOAT

#define SIZEOF_HSPTR            SIZEOF_VOID_P
#define ALIGNMENT_HSPTR         ALIGNMENT_VOID_P

#define SIZEOF_HSFUNPTR         SIZEOF_VOID_P
#define ALIGNMENT_HSFUNPTR      ALIGNMENT_VOID_P

#define SIZEOF_HSSTABLEPTR      SIZEOF_VOID_P
#define ALIGNMENT_HSSTABLEPTR   ALIGNMENT_VOID_P

#define SIZEOF_INT8             SIZEOF_INT8_T
#define ALIGNMENT_INT8          ALIGNMENT_INT8_T

#define SIZEOF_WORD8            SIZEOF_UINT8_T
#define ALIGNMENT_WORD8         ALIGNMENT_UINT8_T

#define SIZEOF_INT16            SIZEOF_INT16_T
#define ALIGNMENT_INT16         ALIGNMENT_INT16_T

#define SIZEOF_WORD16           SIZEOF_UINT16_T
#define ALIGNMENT_WORD16        ALIGNMENT_UINT16_T

#define SIZEOF_INT32            SIZEOF_INT32_T
#define ALIGNMENT_INT32         ALIGNMENT_INT32_T

#define SIZEOF_WORD32           SIZEOF_UINT32_T
#define ALIGNMENT_WORD32        ALIGNMENT_UINT32_T

#define SIZEOF_INT64            SIZEOF_INT64_T
#define ALIGNMENT_INT64         ALIGNMENT_INT64_T

#define SIZEOF_WORD64           SIZEOF_UINT64_T
#define ALIGNMENT_WORD64        ALIGNMENT_UINT64_T

#if !defined(WORD_SIZE_IN_BITS)
#if SIZEOF_HSWORD == 4
#define WORD_SIZE_IN_BITS       32
#define WORD_SIZE_IN_BITS_FLOAT 32.0
#else
#define WORD_SIZE_IN_BITS       64
#define WORD_SIZE_IN_BITS_FLOAT 64.0
#endif
#endif

#if !defined(TAG_BITS)
#if SIZEOF_HSWORD == 4
#define TAG_BITS                2
#else
#define TAG_BITS                3
#endif
#endif

#define TAG_MASK ((1 << TAG_BITS) - 1)

