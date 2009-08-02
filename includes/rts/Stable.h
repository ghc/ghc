/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2004
 *
 * Stable Pointers
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_STABLE_H
#define RTS_STABLE_H

EXTERN_INLINE StgPtr deRefStablePtr (StgStablePtr stable_ptr);
StgStablePtr getStablePtr  (StgPtr p);

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

EXTERN_INLINE
StgPtr deRefStablePtr(StgStablePtr sp)
{
    ASSERT(stable_ptr_table[(StgWord)sp].ref > 0);
    return stable_ptr_table[(StgWord)sp].addr;
}

#endif /* RTS_STABLE_H */
