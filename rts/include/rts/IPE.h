/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2017-2021
 *
 * IPE API
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

typedef struct InfoProv_ {
    char *table_name;
    char *closure_desc;
    char *ty_desc;
    char *label;
    char *module;
    char *srcloc;
} InfoProv;

typedef struct InfoProvEnt_ {
    StgInfoTable *info;
    InfoProv prov;
} InfoProvEnt;

void registerInfoProvList(InfoProvEnt **cc_list);
InfoProvEnt *lookupIPE(const StgInfoTable *info);
