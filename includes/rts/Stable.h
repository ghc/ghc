/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Stable Pointers
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
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
    StgClosure *sn_obj;		/* the StableName object (or NULL) */
} snEntry;

typedef struct {
    StgPtr addr;
} spEntry;

extern DLL_IMPORT_RTS snEntry *stable_name_table;
extern DLL_IMPORT_RTS spEntry *stable_ptr_table;

EXTERN_INLINE
StgPtr deRefStablePtr(StgStablePtr sp)
{
    return stable_ptr_table[(StgWord)sp].addr;
}

#endif /* RTS_STABLE_H */
