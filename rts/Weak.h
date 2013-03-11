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

#include "BeginPrivate.h"

extern rtsBool running_finalizers;
extern StgWeak * dead_weak_ptr_list;

void runCFinalizers(StgCFinalizerList *list);
void runAllCFinalizers(StgWeak *w);
void scheduleFinalizers(Capability *cap, StgWeak *w);
void markWeakList(void);

#include "EndPrivate.h"

#endif /* WEAK_H */

