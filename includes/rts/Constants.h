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
 * DerivedConstants.h by a C program (mkDerivedConstantsHdr).
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_CONSTANTS_H
#define RTS_CONSTANTS_H

/* -----------------------------------------------------------------------------
   Minimum closure sizes

   This is the minimum number of words in the payload of a
   heap-allocated closure, so that the closure has enough room to be
   overwritten with a forwarding pointer during garbage collection.
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
#define MAX_INTLIKE             16
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
   How much Haskell stack space to reserve for the saving of registers
   etc. in the case of a stack/heap overflow.

   This must be large enough to accomodate the largest stack frame
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
#define MBLOCK_SHIFT   20

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
 * NB. keep these in sync with GHC/Conc.lhs: threadStatus
 */
#define NotBlocked          0
#define BlockedOnMVar       1
#define BlockedOnMVarRead   2
#define BlockedOnBlackHole  3
#define BlockedOnRead       4
#define BlockedOnWrite      5
#define BlockedOnDelay      6
#define BlockedOnSTM        7

/* Win32 only: */
#define BlockedOnDoProc     8

/* Only relevant for PAR: */
  /* blocked on a remote closure represented by a Global Address: */
#define BlockedOnGA         9
  /* same as above but without sending a Fetch message */
#define BlockedOnGA_NoSend  10
/* Only relevant for THREADED_RTS: */
#define BlockedOnCCall      11
#define BlockedOnCCall_Interruptible 12
   /* same as above but permit killing the worker thread */

/* Involved in a message sent to tso->msg_cap */
#define BlockedOnMsgThrowTo 13

/* The thread is not on any run queues, but can be woken up
   by tryWakeupThread() */
#define ThreadMigrating     14

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

#endif /* RTS_CONSTANTS_H */
