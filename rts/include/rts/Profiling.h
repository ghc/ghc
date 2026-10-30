/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2017-2018
 *
 * Cost-centre profiling API
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

RTS_PUBLIC void registerCcList(CostCentre **cc_list);
RTS_PUBLIC void registerCcsList(CostCentreStack **cc_list);
