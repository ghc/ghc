/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2017-2018
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

void registerInfoProvList(InfoProvEnt **cc_list);
InfoProvEnt * lookupIPE(StgClosure *info);