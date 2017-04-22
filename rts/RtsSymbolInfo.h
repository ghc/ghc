/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbol Info
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "LinkerInternals.h"

HsBool isSymbolWeak(ObjectCode *owner, void *label);
void setWeakSymbol(ObjectCode *owner, void *label);
