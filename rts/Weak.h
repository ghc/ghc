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

extern rtsBool running_finalizers;

void runCFinalizer(StgVoid *fn, StgVoid *ptr, StgVoid *env, StgWord flag);
void runAllCFinalizers(StgWeak *w);
void scheduleFinalizers(Capability *cap, StgWeak *w);
void markWeakList(void);

#endif
