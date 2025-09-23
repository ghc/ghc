/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Stable Pointers
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

ATTR_ALWAYS_INLINE EXTERN_INLINE StgPtr deRefStablePtr (StgStablePtr stable_ptr);
StgStablePtr getStablePtr  (StgPtr p);

/* -----------------------------------------------------------------------------
   PRIVATE from here.
   -------------------------------------------------------------------------- */

typedef struct {
    StgPtr addr;         // Haskell object when entry is in use, next free
                         // entry (NULL when this is the last free entry)
                         // otherwise.
} spEntry;

extern spEntry *stable_ptr_table;

ATTR_ALWAYS_INLINE EXTERN_INLINE
StgPtr deRefStablePtr(StgStablePtr sp)
{
    // see Note [NULL StgStablePtr]
    if (sp == 0) {
        return NULL;
    }
    StgWord spw = (StgWord)sp - 1;
    // acquire load to ensure that we see the new SPT if it has been recently
    // enlarged.
    const spEntry *spt = ACQUIRE_LOAD(&stable_ptr_table);
    // acquire load to ensure that the referenced object is visible.
    return ACQUIRE_LOAD(&spt[spw].addr);
}
