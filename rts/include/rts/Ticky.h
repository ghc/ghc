/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * TICKY_TICKY types
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

/* -----------------------------------------------------------------------------
   The StgEntCounter type - needed regardless of TICKY_TICKY
   -------------------------------------------------------------------------- */

typedef struct _StgEntCounter {
  /* Using StgWord for everything, because both the C and asm code
     generators make trouble if you try to pack things tighter */
    StgWord     registeredp;    /* 0 == no, 1 == yes */
    StgInt      arity;          /* arity (static info) */
    StgInt      allocd;         /* # allocation of this closure */
                                /* (rest of args are in registers) */
    char        *str;           /* name of the thing */
    char        *arg_kinds;     /* info about the args types */
    char        *ticky_json;    /* json_info for eventlog mostly describing the tick */
    StgInfoTable *info;         /* Info table corresponding to this closure */
    StgInt      entry_count;    /* Trips to fast entry code */
    StgInt      allocs;         /* number of allocations by this fun */
    struct _StgEntCounter *link;/* link to chain them all together */
} StgEntCounter;

void requestTickyCounterSamples(void);
