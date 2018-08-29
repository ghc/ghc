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

#pragma once

EXTERN_INLINE StgPtr deRefStablePtr (StgStablePtr stable_ptr);
StgStablePtr getStablePtr  (StgPtr p);

/* -----------------------------------------------------------------------------
   PRIVATE from here.
   -------------------------------------------------------------------------- */

typedef struct {
    StgPtr addr;         // Haskell object when entry is in use, next free
                         // entry (NULL when this is the last free entry)
                         // otherwise.
} spEntry;

extern DLL_IMPORT_RTS spEntry *stable_ptr_table;

EXTERN_INLINE
StgPtr deRefStablePtr(StgStablePtr sp)
{
    return stable_ptr_table[(StgWord)sp].addr;
}
