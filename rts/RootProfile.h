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

#include "BeginPrivate.h"

void rootProfile(Time t, Census *census);

StgWord setRootProfPtrs(StgWord n, HsStablePtr *sps, const char** descs);

// For GC.c
extern traverseState g_rootTraverseState;

#include "EndPrivate.h"

#endif
