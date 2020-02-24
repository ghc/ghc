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

void endRootProfiling(void);

void rootProfile(Time t, Census *census);

bool rootProfileWasClosureVisited(const StgClosure *c);

const void *rootProfileGetClosureIdentity(const StgClosure *c);

const char *rootProfileMkClosureLabel(Arena *arena, const void *key);

// For GC.c
extern traverseState g_rootTraverseState;

#include "EndPrivate.h"

#endif
