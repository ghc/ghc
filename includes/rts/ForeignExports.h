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

struct ForeignExportsList {
    struct ForeignExportsList *next;
    struct _ObjectCode *oc;
    int n_entries;
    StgPtr exports[];
};

void registerForeignExports(struct ForeignExportsList *exports);

