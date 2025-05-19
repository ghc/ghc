/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Constants
 *
 * NOTE: this information is used by both the compiler and the RTS.
 * Some of it is tweakable, and some of it must be kept up to date
 * with various other parts of the system.
 *
 * Constants which are derived automatically from other definitions in
 * the system (eg. structure sizes) are generated into the file
 * DerivedConstants.h by a C program (utils/deriveConstants).
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

/* -----------------------------------------------------------------------------
   Minimum closure sizes

   This is the minimum number of words in the payload of a heap-allocated
   closure, so that the closure has two bits in the bitmap for mark-compact
   collection.

   See Note [Mark bits in mark-compact collector] in rts/sm/Compact.h
   -------------------------------------------------------------------------- */

#define MIN_PAYLOAD_SIZE 1

/* -----------------------------------------------------------------------------
   Constants to do with specialised closure types.
   -------------------------------------------------------------------------- */

/* We have some pre-compiled selector thunks defined in rts/StgStdThunks.hc.
 * This constant defines the highest selectee index that we can replace with a
 * reference to the pre-compiled code.
 */

#define MAX_SPEC_SELECTEE_SIZE 15

/* Vector-apply thunks.  These thunks just push their free variables
 * on the stack and enter the first one.  They're a bit like PAPs, but
 * don't have a dynamic size.  We've pre-compiled a few to save
 * space.
 */

#define MAX_SPEC_AP_SIZE       7

/* Specialised FUN/THUNK/CONSTR closure types */

#define MAX_SPEC_THUNK_SIZE    2
#define MAX_SPEC_FUN_SIZE      2
#define MAX_SPEC_CONSTR_SIZE   2

/* Range of built-in table of static small int-like and char-like closures.
 *
 *   NB. This corresponds with the number of actual INTLIKE/CHARLIKE
 *   closures defined in rts/StgMiscClosures.cmm.
 */
#define MAX_INTLIKE             255
#define MIN_INTLIKE             (-16)

#define MAX_CHARLIKE            255
#define MIN_CHARLIKE            0

/* Each byte in the card table for an StgMutaArrPtrs covers
 * (1<<MUT_ARR_PTRS_CARD_BITS) elements in the array.  To find a good
 * value for this, I used the benchmarks nofib/gc/hash,
 * nofib/gc/graph, and nofib/gc/gc_bench.
 */
#define MUT_ARR_PTRS_CARD_BITS 7

/* -----------------------------------------------------------------------------
   STG Registers.

   Note that in MachRegs.h we define how many of these registers are
   *real* machine registers, and not just offsets in the Register Table.
   -------------------------------------------------------------------------- */

#define MAX_VANILLA_REG 10
#define MAX_FLOAT_REG   6
#define MAX_DOUBLE_REG  6
#define MAX_LONG_REG    1
#define MAX_XMM_REG     6

/* -----------------------------------------------------------------------------
   Semi-Tagging constants

   Old Comments about this stuff:

   Tags for indirection nodes and ``other'' (probably unevaluated) nodes;
   normal-form values of algebraic data types will have tags 0, 1, ...

   @INFO_IND_TAG@ is different from @INFO_OTHER_TAG@ just so we can count
   how often we bang into indirection nodes; that's all.  (WDP 95/11)

   ToDo: find out if we need any of this.
   -------------------------------------------------------------------------- */

#define INFO_OTHER_TAG          (-1)
#define INFO_IND_TAG            (-2)
#define INFO_FIRST_TAG          0

/* -----------------------------------------------------------------------------
   How much C stack to reserve for local temporaries when in the STG
   world.  Used in StgCRun.c.
   -------------------------------------------------------------------------- */

#define RESERVED_C_STACK_BYTES (2048 * SIZEOF_LONG)

/* -----------------------------------------------------------------------------
   How large is the stack frame saved by StgRun?
   world.  Used in StgCRun.c.

   The size has to be enough to save the registers (see StgCRun)
   plus padding if the result is not 16 byte aligned.
   See the Note [Stack Alignment on X86] in StgCRun.c for details.

   -------------------------------------------------------------------------- */
#if defined(x86_64_HOST_ARCH)
#  if defined(mingw32_HOST_OS)
#    define STG_RUN_STACK_FRAME_SIZE 240
#  else
#    define STG_RUN_STACK_FRAME_SIZE 48
#  endif
#endif

/* -----------------------------------------------------------------------------
   StgRun related labels shared between StgCRun.c and StgStartup.cmm.
   -------------------------------------------------------------------------- */

#if defined(LEADING_UNDERSCORE)
#define STG_RUN "_StgRun"
#define STG_RUN_JMP _StgRunJmp
#define STG_RETURN "_StgReturn"
#else
#define STG_RUN "StgRun"
#define STG_RUN_JMP StgRunJmp
#define STG_RETURN "StgReturn"
#endif

/* -----------------------------------------------------------------------------
   How much Haskell stack space to reserve for the saving of registers
   etc. in the case of a stack/heap overflow.

   This must be large enough to accommodate the largest stack frame
   pushed in one of the heap check fragments in HeapStackCheck.hc
   (ie. currently the generic heap checks - 3 words for StgRetDyn,
   18 words for the saved registers, see StgMacros.h).
   -------------------------------------------------------------------------- */

#define RESERVED_STACK_WORDS 21

/* -----------------------------------------------------------------------------
   The limit on the size of the stack check performed when we enter an
   AP_STACK, in words.  See raiseAsync() and bug #1466.
   -------------------------------------------------------------------------- */

#define AP_STACK_SPLIM 1024

/* -----------------------------------------------------------------------------
   Storage manager constants
   -------------------------------------------------------------------------- */

/* The size of a block (2^BLOCK_SHIFT bytes) */
#define BLOCK_SHIFT  12

/* The size of a megablock (2^MBLOCK_SHIFT bytes) */
#if defined(wasm32_HOST_ARCH)
#define MBLOCK_SHIFT   16
#else
#define MBLOCK_SHIFT   20
#endif

/* -----------------------------------------------------------------------------
   Bitmap/size fields (used in info tables)
   -------------------------------------------------------------------------- */

/* In a 32-bit bitmap field, we use 5 bits for the size, and 27 bits
 * for the bitmap.  If the bitmap requires more than 27 bits, then we
 * store it in a separate array, and leave a pointer in the bitmap
 * field.  On a 64-bit machine, the sizes are extended accordingly.
 */
#if SIZEOF_VOID_P == 4
#define BITMAP_SIZE_MASK     0x1f
#define BITMAP_BITS_SHIFT    5
#elif SIZEOF_VOID_P == 8
#define BITMAP_SIZE_MASK     0x3f
#define BITMAP_BITS_SHIFT    6
#else
#error unknown SIZEOF_VOID_P
#endif

/* -----------------------------------------------------------------------------
   Lag/Drag/Void constants
   -------------------------------------------------------------------------- */

/*
  An LDV word is divided into 3 parts: state bits (LDV_STATE_MASK), creation
  time bits (LDV_CREATE_MASK), and last use time bits (LDV_LAST_MASK).
 */
#if SIZEOF_VOID_P == 8
#define LDV_SHIFT               30
#define LDV_STATE_MASK          0x1000000000000000
#define LDV_CREATE_MASK         0x0FFFFFFFC0000000
#define LDV_LAST_MASK           0x000000003FFFFFFF
#define LDV_STATE_CREATE        0x0000000000000000
#define LDV_STATE_USE           0x1000000000000000
#else
#define LDV_SHIFT               15
#define LDV_STATE_MASK          0x40000000
#define LDV_CREATE_MASK         0x3FFF8000
#define LDV_LAST_MASK           0x00007FFF
#define LDV_STATE_CREATE        0x00000000
#define LDV_STATE_USE           0x40000000
#endif /* SIZEOF_VOID_P */

/* See Note [Debugging predicates for pointers] in ClosureMacros.h */
#if !defined(INVALID_GHC_POINTER)
#if !defined(DEBUG)
#define INVALID_GHC_POINTER 0x0
#elif SIZEOF_VOID_P== 4
/* N.B. this may result in false-negatives from LOOKS_LIKE_PTR on some
 * platforms since this is a valid user-space address.
 */
#define INVALID_GHC_POINTER 0xaaaaaaaa
#else
/* N.B. this is typically a kernel-mode address on 64-bit platforms */
#define INVALID_GHC_POINTER 0xaaaaaaaaaaaaaaaa
#endif
#endif

/* -----------------------------------------------------------------------------
   TSO related constants
   -------------------------------------------------------------------------- */

/*
 * Constants for the what_next field of a TSO, which indicates how it
 * is to be run.
 */
#define ThreadRunGHC    1       /* return to address on top of stack */
#define ThreadInterpret 2       /* interpret this thread */
#define ThreadKilled    3       /* thread has died, don't run it */
#define ThreadComplete  4       /* thread has finished */

/*
 * Constants for the why_blocked field of a TSO
 * NB. keep these in sync with GHC/Conc/Sync.hs: threadStatus
 */
#define NotBlocked          0
#define BlockedOnMVar       1
#define BlockedOnMVarRead   14 /* TODO: renumber me, see #9003 */
#define BlockedOnBlackHole  2
#define BlockedOnRead       3
#define BlockedOnWrite      4
#define BlockedOnDelay      5
#define BlockedOnSTM        6

/* Win32 only: */
#define BlockedOnDoProc     7

/* Only relevant for THREADED_RTS: */
#define BlockedOnCCall      10
#define BlockedOnCCall_Interruptible 11
   /* same as above but permit killing the worker thread */

/* Involved in a message sent to tso->msg_cap */
#define BlockedOnMsgThrowTo 12

/* The thread is not on any run queues, but can be woken up
   by tryWakeupThread() */
#define ThreadMigrating     13

/* Next number is 15.  */

/*
 * These constants are returned to the scheduler by a thread that has
 * stopped for one reason or another.  See typedef StgThreadReturnCode
 * in TSO.h.
 */
#define HeapOverflow   1                /* might also be StackOverflow */
#define StackOverflow  2
#define ThreadYielding 3
#define ThreadBlocked  4
#define ThreadFinished 5

/*
 * Flags for the tso->flags field.
 */

/*
 * TSO_LOCKED is set when a TSO is locked to a particular Capability.
 */
#define TSO_LOCKED  2

/*
 * TSO_BLOCKEX: the TSO is blocking exceptions
 *
 * TSO_INTERRUPTIBLE: the TSO can be interrupted if it blocks
 * interruptibly (eg. with BlockedOnMVar).
 *
 * TSO_STOPPED_ON_BREAKPOINT: the thread is currently stopped in a breakpoint
 */
#define TSO_BLOCKEX       4
#define TSO_INTERRUPTIBLE 8
#define TSO_STOPPED_ON_BREAKPOINT 16

/*
 * Used by the sanity checker to check whether TSOs are on the correct
 * mutable list.
 */
#define TSO_MARKED 64

/*
 * Used to communicate between stackSqueeze() and
 * threadStackOverflow() that a thread's stack was squeezed and the
 * stack may not need to be expanded.
 */
#define TSO_SQUEEZED 128

/*
 * Enables the AllocationLimitExceeded exception when the thread's
 * allocation limit goes negative.
 */
#define TSO_ALLOC_LIMIT 256

/*
 * Enables step-in mode for this thread -- it will stop at the immediate next
 * breakpoint found in this thread.
 */
#define TSO_STOP_NEXT_BREAKPOINT 512

/*
 * The number of times we spin in a spin lock before yielding (see
 * #3758).  To tune this value, use the benchmark in #3758: run the
 * server with -N2 and the client both on a dual-core.  Also make sure
 * that the chosen value doesn't slow down any of the parallel
 * benchmarks in nofib/parallel.
 */
#define SPIN_COUNT 1000

/* -----------------------------------------------------------------------------
   Spare workers per Capability in the threaded RTS

   No more than MAX_SPARE_WORKERS will be kept in the thread pool
   associated with each Capability.
   -------------------------------------------------------------------------- */

#define MAX_SPARE_WORKERS 6

/*
 * The maximum number of NUMA nodes we support.  This is a fixed limit so that
 * we can have static arrays of this size in the RTS for speed.
 */
#define MAX_NUMA_NODES 16

/*
 * closure_desc of InfoProv is now uint32_t at all platforms, but
 * we have to keep its stringified representation.
 * It is known that maximum length of uint32_t in string is 10 chars (4294967295) + 1 NULL.
 */
#define CLOSURE_DESC_BUFFER_SIZE 11
