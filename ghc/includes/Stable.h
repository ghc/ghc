/* -----------------------------------------------------------------------------
 * $Id: Stable.h,v 1.8 2001/07/23 17:23:19 simonmar Exp $
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
 * ----------------------------------------------------------------------------*/

#define STABLEPTR_WEIGHT_MASK   ((StgWord)0xff << ((sizeof(StgWord)-1) * BITS_PER_BYTE))
#define STABLEPTR_WEIGHT_SHIFT  (BITS_IN(StgWord) - 8)

/* -----------------------------------------------------------------------------
   External C Interface
   -------------------------------------------------------------------------- */

extern StgPtr         deRefStablePtr(StgStablePtr stable_ptr);
extern void           freeStablePtr(StgStablePtr sp);
extern StgStablePtr   splitStablePtr(StgStablePtr sp);

/* -----------------------------------------------------------------------------
   PRIVATE from here.
   -------------------------------------------------------------------------- */

extern StgStablePtr getStablePtr(StgPtr p);

typedef struct { 
  StgPtr  addr;			/* Haskell object, free list, or NULL */
  StgPtr  old;			/* old Haskell object, used during GC */
  StgWord weight;		/* used for reference counting */
  StgClosure *sn_obj;		/* the StableName object (or NULL) */
} snEntry;

extern DLL_IMPORT_RTS snEntry *stable_ptr_table;
extern DLL_IMPORT_RTS snEntry *stable_ptr_free;

extern DLL_IMPORT_RTS unsigned int SPT_size;

extern inline StgPtr
deRefStablePtr(StgStablePtr sp)
{
  ASSERT(stable_ptr_table[stgCast(StgWord,sp) & ~STABLEPTR_WEIGHT_MASK].weight > 0);
  return stable_ptr_table[stgCast(StgWord,sp) & ~STABLEPTR_WEIGHT_MASK].addr;
}

extern inline void
freeStablePtr(StgStablePtr sp)
{
  StgWord sn = stgCast(StgWord,sp) & ~STABLEPTR_WEIGHT_MASK;
  
  ASSERT(sn < SPT_size
	 && stable_ptr_table[sn].addr != NULL
	 && stable_ptr_table[sn].weight > 0);
  
  stable_ptr_table[sn].weight += 
      1 << ((((StgWord)sp & STABLEPTR_WEIGHT_MASK) >> STABLEPTR_WEIGHT_SHIFT) - 1);
}

extern inline StgStablePtr
splitStablePtr(StgStablePtr sp)
{
  /* doesn't need access to the stable pointer table */
  StgWord weight = (stgCast(StgWord,sp) & STABLEPTR_WEIGHT_MASK) / 2;
  return stgCast(StgStablePtr,(stgCast(StgWord,sp) & ~STABLEPTR_WEIGHT_MASK) + weight);
}

/* No deRefStableName, because the existence of a stable name doesn't
 * guarantee the existence of the object itself.
 */
