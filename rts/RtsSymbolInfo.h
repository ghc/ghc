/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbol Info
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_SYMBOLINFO_H
#define RTS_SYMBOLINFO_H

#include "LinkerInternals.h"

HsBool isSymbolWeak(ObjectCode *owner, void *label);
void setWeakSymbol(ObjectCode *owner, void *label);

#endif /* RTS_SYMBOLINFO_H */
