/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2018
 *
 * Non-moving garbage collector and allocator
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "NonMoving.h"

#include "BeginPrivate.h"

void *nonmovingAllocate(Capability *cap, StgWord sz);
void *nonmovingAllocateGC(Capability *cap, StgWord sz);
void nonmovingInitCapability(Capability *cap);

#include "EndPrivate.h"
