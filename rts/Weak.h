/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Weak pointers / finalizers
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "Capability.h"

#include "BeginPrivate.h"

extern bool running_finalizers;
extern StgWeak * dead_weak_ptr_list;

void runCFinalizers(StgCFinalizerList *list);
void runAllCFinalizers(StgWeak *w);
void scheduleFinalizers(Capability *cap, StgWeak *w);
void markWeakList(void);
bool runSomeFinalizers(bool all);

#include "EndPrivate.h"
