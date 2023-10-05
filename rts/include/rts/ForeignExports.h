/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2009
 *
 * Interface to the RTS's foreign export tracking code.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

struct _ObjectCode;

/* N.B. See Note [Tracking foreign exports] in
 * rts/ForeignExports.c. */
struct ForeignExportsList {
      /* a link field for linking these together into lists.
       */
    struct ForeignExportsList *next;
      /* the length of ->exports */
    int n_entries;
      /* if the RTS linker loaded the module,
       * to which ObjectCode these exports belong. */
    struct _ObjectCode *oc;
      /* if the RTS linker loaded the module,
       * this points to an array of length ->n_entries
       * recording the StablePtr for each export. */
    StgStablePtr **stable_ptrs;
      /* the exported closures. of length ->exports. */
    StgPtr exports[];
};

void registerForeignExports(struct ForeignExportsList *exports);

