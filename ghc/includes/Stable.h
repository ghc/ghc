/* -----------------------------------------------------------------------------
 * $Id: Stable.h,v 1.14 2002/12/19 14:25:04 simonmar Exp $
 *
 * (c) The GHC Team, 1998-2000
 *
 * Stable Pointers: A stable pointer is represented as an index into
 * the stable pointer table in the low BITS_PER_WORD-8 bits with a
 * weight in the upper 8 bits.
 *
 * SUP: StgStablePtr used to be a synonym for StgWord, but stable pointers
 * are guaranteed to be void* on the C-side, so we have to do some occasional
 * casting. Size is not a matter, because StgWord is always the same size as
 * a void*.
 *
 * ---------------------------------------------------------------------------*/

#ifndef STABLE_H
#define STABLE_H

/* -----------------------------------------------------------------------------
   External C Interface
   -------------------------------------------------------------------------- */

extern StgPtr         deRefStablePtr(StgStablePtr stable_ptr);
extern void           freeStablePtr(StgStablePtr sp);
extern StgStablePtr   splitStablePtr(StgStablePtr sp);
extern StgStablePtr   getStablePtr(StgPtr p);

/* -----------------------------------------------------------------------------
   PRIVATE from here.
   -------------------------------------------------------------------------- */

typedef struct { 
  StgPtr  addr;			/* Haskell object, free list, or NULL */
  StgPtr  old;			/* old Haskell object, used during GC */
  StgWord ref;			/* used for reference counting */
  StgClosure *sn_obj;		/* the StableName object (or NULL) */
} snEntry;

extern DLL_IMPORT_RTS snEntry *stable_ptr_table;

extern void freeStablePtr(StgStablePtr sp);

#ifndef RTS_STABLE_C
extern inline
#endif
StgPtr deRefStablePtr(StgStablePtr sp)
{
    ASSERT(stable_ptr_table[(StgWord)sp].ref > 0);
    return stable_ptr_table[(StgWord)sp].addr;
}

/* No deRefStableName, because the existence of a stable name doesn't
 * guarantee the existence of the object itself.
 */

#endif
