/* ----------------------------------------------------------------------------
 * $Id: Constants.h,v 1.20 2001/11/28 14:31:27 simonmar Exp $
 *
 * (c) The GHC Team, 1998-1999
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
 * -------------------------------------------------------------------------- */

#ifndef CONSTANTS_H
#define CONSTANTS_H

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
   STG Registers.

   Note that in MachRegs.h we define how many of these registers are
   *real* machine registers, and not just offsets in the Register Table.
   -------------------------------------------------------------------------- */

#define MAX_VANILLA_REG 8
#define MAX_FLOAT_REG   4
#define MAX_DOUBLE_REG  2
/* register is only used for returning (unboxed) 64-bit vals */
#define MAX_LONG_REG    1

/*---- Maximum number of constructors in a data type for direct-returns.  */

#define MAX_VECTORED_RTN 8

/*---- Range of built-in table of static small int-like and char-like closures. */

#define MAX_INTLIKE 		16
#define MIN_INTLIKE 		(-16)

#define MAX_CHARLIKE		255
#define MIN_CHARLIKE		0

/* You can change these constants (I hope) but be sure to modify
   rts/StgMiscClosures.hs accordingly. */

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
   How much C stack to reserve for local temporaries when in the STG
   world.  Used in StgRun.S and StgCRun.c.
   -------------------------------------------------------------------------- */

#define RESERVED_C_STACK_BYTES (2048 * SIZEOF_LONG)

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

/* The size of a block (2^BLOCK_SHIFT bytes) */
#define BLOCK_SHIFT  12

/* The size of a megablock (2^MBLOCK_SHIFT bytes) */
#define MBLOCK_SHIFT   20

/* the largest size an object can be before we give it a block of its
 * own and treat it as an immovable object during GC, expressed as a
 * fraction of BLOCK_SIZE.
 */
#define LARGE_OBJECT_THRESHOLD ((nat)(BLOCK_SIZE * 8 / 10))

#endif /* CONSTANTS_H */

