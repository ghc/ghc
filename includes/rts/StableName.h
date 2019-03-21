/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * Stable Names
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

/* -----------------------------------------------------------------------------
   PRIVATE from here.
   -------------------------------------------------------------------------- */

typedef struct {
    StgPtr  addr;        // Haskell object when entry is in use, next free
                         // entry (NULL when this is the last free entry)
                         // otherwise. May be NULL temporarily during GC (when
                         // pointee dies).

    StgPtr  old;         // Old Haskell object, used during GC

    StgClosure *sn_obj;  // The StableName object, or NULL when the entry is
                         // free
} snEntry;

extern DLL_IMPORT_RTS snEntry *stable_name_table;
