/* ----------------------------------------------------------------------------
 * $Id: Constants.h,v 1.4 1999/01/26 16:16:20 simonm Exp $
 *
 * Constants
 *
 * NOTE: this information is used by both the compiler and the RTS,
 * and *must* be kept up-to-date with respect to the rest of the
 * world.
 *
 * -------------------------------------------------------------------------- */

#ifndef CONSTANTS_H
#define CONSTANTS_H

/* -----------------------------------------------------------------------------
   Header Sizes

   NOTE: keep these in line with the real definitions in Closures.h
   -------------------------------------------------------------------------- */

#define STD_HDR_SIZE   1
#define PROF_HDR_SIZE  1
#define GRAN_HDR_SIZE  1
#define TICKY_HDR_SIZE 0

#define ARR_HDR_SIZE   1

/* -----------------------------------------------------------------------------
   Info Table sizes

   The native code generator needs to know these things, and can't use
   the C sizeof() function.
  
   NOTE: keep these in line with the real definitions in InfoTables.h

   NOTE: the PROF, and GRAN values are *wrong*  (ToDo)
   -------------------------------------------------------------------------- */

#define STD_ITBL_SIZE   3
#define PROF_ITBL_SIZE  1
#define GRAN_ITBL_SIZE  1
#define TICKY_ITBL_SIZE 0

/* -----------------------------------------------------------------------------
   Minimum closure sizes

   Here We define the minimum size for updatable closures. This must be at
   least 2, to allow for cons cells and linked indirections. All updates
   will be performed on closures of this size. For non-updatable closures
   the minimum size is 1 to allow for a forwarding pointer.

   Linked indirections are UPD_OLDGEN things: see Closures.h

   o MIN_UPD_SIZE doesn't apply to stack closures, static closures
     or non-updateable objects like PAPs or CONSTRs
   o MIN_UPD_SIZE is big enough to contain any of the following:
     o EVACUATED
     o BLACKHOLE
     o BLOCKING QUEUE
     o IND, IND_PERM, IND_OLDGEN and IND_OLDGEN_PERM
       (it need not be big enough for IND_STATIC - but it is)
   o MIN_NONUPD_SIZE doesn't apply to stack closures, static closures
     or updateable objects like APs, THUNKS or THUNK_SELECTORs
   o MIN_NONUPD_SIZE is big enough to contain any of the following:
     o EVACUATED
   -------------------------------------------------------------------------- */

#define MIN_UPD_SIZE	2
#define MIN_NONUPD_SIZE 1

/* -----------------------------------------------------------------------------
   Constants to do with specialised closure types.
   -------------------------------------------------------------------------- */

/* We have some pre-compiled selector thunks defined in
 * StgSelectors.hc in the runtime system.  This constant defines the
 * highest selectee index that we can replace with a reference to the
 * pre-compiled code.
 */

#define MAX_SPEC_SELECTEE_SIZE 15

/* Vector-apply thunks.  These thunks just push their free variables
 * on the stack and enter the first one.  They're a bit like PAPs, but
 * don't have a dynamic size.  We've pre-compiled a few to save
 * space. 
 */

#define MAX_SPEC_AP_SIZE       8

/* Specialised FUN/THUNK/CONSTR closure types */

#define MAX_SPEC_THUNK_SIZE    2
#define MAX_SPEC_FUN_SIZE      2
#define MAX_SPEC_CONSTR_SIZE   2

/* -----------------------------------------------------------------------------
   Update Frame Layout
   -------------------------------------------------------------------------- */

#define NOSCC_UF_SIZE 	3
#define SCC_UF_SIZE	4

#if defined(PROFILING)
#define UF_SIZE	SCC_UF_SIZE
#else
#define UF_SIZE NOSCC_UF_SIZE
#endif

#define UF_RET		0
#define UF_SU		1
#define UF_UPDATEE	2
#define UF_CCS		3

/* -----------------------------------------------------------------------------
   SEQ frame size
   -------------------------------------------------------------------------- */

#if defined(PROFILING)
#define SEQ_FRAME_SIZE 3
#else
#define SEQ_FRAME_SIZE 2
#endif

/* -----------------------------------------------------------------------------
   STG Registers.

   Note that in MachRegs.h we define how many of these registers are
   *real* machine registers, and not just offsets in the Register Table.
   -------------------------------------------------------------------------- */

#define MAX_VANILLA_REG 8
#define MAX_FLOAT_REG   4
#define MAX_DOUBLE_REG  2
/* register is only used for returning (unboxed) 64-bit vals */
#define MAX_LONG_REG    1

/*---- The size of an StgDouble, in StgWords. */

#if SIZEOF_VOID_P == SIZEOF_DOUBLE
#define DOUBLE_SIZE 	1
#else
#define DOUBLE_SIZE 	2
#endif

/*---- The size of Stg{Int,Word}64e, in StgWords. */
#if SIZEOF_VOID_P == 8
#define WORD64_SIZE 	1
#define INT64_SIZE 	1
#else
#define WORD64_SIZE 	2
#define INT64_SIZE 	2
#endif

/*---- Maximum number of constructors in a data type for direct-returns.  */

#define MAX_VECTORED_RTN 8

/*---- Range of built-in table of static small int-like closures. */

#define MAX_INTLIKE 		(16)
#define MIN_INTLIKE 		(-16)

/*---- Minimum number of words left in heap after GC to carry on */

#define HEAP_HWM_WORDS	1024

/* -----------------------------------------------------------------------------
   Semi-Tagging constants

   Old Comments about this stuff:

   Tags for indirection nodes and ``other'' (probably unevaluated) nodes;
   normal-form values of algebraic data types will have tags 0, 1, ...
   
   @INFO_IND_TAG@ is different from @INFO_OTHER_TAG@ just so we can count
   how often we bang into indirection nodes; that's all.  (WDP 95/11)

   ToDo: find out if we need any of this.
   -------------------------------------------------------------------------- */

#define INFO_OTHER_TAG		(-1)
#define INFO_IND_TAG		(-2)
#define INFO_FIRST_TAG		0

/* -----------------------------------------------------------------------------
   Context switch timing constants.
   -------------------------------------------------------------------------- */

#define CS_MAX_FREQUENCY 100              /* context switches per second */
#define CS_MIN_MILLISECS (1000/CS_MAX_FREQUENCY)/* milliseconds per slice */
 
/* -----------------------------------------------------------------------------
   How much C stack to reserve for local temporaries when in the STG
   world.  Used in StgRun.S and StgCRun.c.
   -------------------------------------------------------------------------- */

#define RESERVED_C_STACK_BYTES (512 * SIZEOF_LONG)

/* -----------------------------------------------------------------------------
   How much Haskell stack space to reserve for the saving of registers
   etc. in the case of a stack/heap overflow.
   
   This must be large enough to accomodate the largest stack frame
   pushed in one of the heap check fragments in HeapStackCheck.hc
   (ie. currently the generic heap checks - 19 words).
   -------------------------------------------------------------------------- */

#define RESERVED_STACK_WORDS 19

/* -----------------------------------------------------------------------------
   Storage manager constants
   -------------------------------------------------------------------------- */

/* The size of a block */
#define BLOCK_SIZE   0x2000
#define BLOCK_SHIFT  13

/* The size of a megablock */
#define MBLOCK_SIZE    0x100000
#define MBLOCK_SHIFT   20

/* the largest size an object can be before we give it a block of its
 * own and treat it as an immovable object during GC, expressed as a
 * fraction of BLOCK_SIZE.
 */
#define LARGE_OBJECT_THRESHOLD ((nat)(BLOCK_SIZE * 8 / 10))

#endif /* CONSTANTS_H */

