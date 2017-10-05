/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2017-2018
 *
 * Cost-centre profiling API
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

void registerCcList(CostCentre **cc_list);
void registerCcsList(CostCentreStack **cc_list);
