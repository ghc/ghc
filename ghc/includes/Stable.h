/* -----------------------------------------------------------------------------
 * $Id: Stable.h,v 1.3 1999/02/26 12:46:45 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Stable names and stable pointers
 *
 * ---------------------------------------------------------------------------*/

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
  StgWord weight;		/* used for reference counting */
  StgClosure *sn_obj;		/* the StableName object (or NULL) */
} snEntry;

extern snEntry *stable_ptr_table;
extern snEntry *stable_ptr_free;

extern unsigned int SPT_size;

extern inline StgPtr
deRefStablePtr(StgStablePtr sp)
{
  ASSERT(stable_ptr_table[sp & ~STABLEPTR_WEIGHT_MASK].weight > 0);
  return stable_ptr_table[sp & ~STABLEPTR_WEIGHT_MASK].addr;
}

extern inline void
freeStablePtr(StgStablePtr sp)
{
  StgWord sn = sp & ~STABLEPTR_WEIGHT_MASK;
  
  ASSERT(sn < SPT_size
	 && stable_ptr_table[sn].addr != NULL
	 && stable_ptr_table[sn].weight > 0);
  
  stable_ptr_table[sn].weight += (sp & STABLEPTR_WEIGHT_MASK) >> STABLEPTR_WEIGHT_SHIFT;
}

extern inline StgStablePtr
splitStablePtr(StgStablePtr sp)
{
  /* doesn't need access to the stable pointer table */
  StgWord weight = (sp & STABLEPTR_WEIGHT_MASK) / 2;
  return (sp & ~STABLEPTR_WEIGHT_MASK) + weight;
}

/* No deRefStableName, because the existence of a stable name doesn't
 * guarantee the existence of the object itself.
 */
