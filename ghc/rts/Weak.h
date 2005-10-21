/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Weak pointers / finalizers
 *
 * ---------------------------------------------------------------------------*/

#ifndef WEAK_H
#define WEAK_H

#include "Capability.h"

void scheduleFinalizers(Capability *cap, StgWeak *w);
void markWeakList(void);

#endif
