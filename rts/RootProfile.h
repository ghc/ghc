/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2019
 * Author: Daniel Gröber
 *
 * Heap traversal profiling.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(PROFILING)

#include "ProfHeapInternal.h"
#include "Arena.h"

#include "BeginPrivate.h"

void rootProfile(traverseState *ts, Time t, Census *census);

void endRootProfiling(traverseState *ts);

bool rootProfileWasClosureVisited(traverseState *ts, const StgClosure *c);

const void *rootProfileGetClosureIdentity(traverseState *ts, const StgClosure *c);

const char *rootProfileMkClosureLabel(Arena *arena, const void *key);

#include "EndPrivate.h"

#endif
