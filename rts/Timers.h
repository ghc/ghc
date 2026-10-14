/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1995-2006, 2026
 *
 * Management for the collection of timers on behalf of other RTS sub-systems.
 *
 * This includes timers for:
 *  + thread context switching (on each capability)
 *  + profiling:
 *    - cost center profiling
 *    - ticky ticky profiling
 *    - heap profiling
 *  + eventlog flushing
 *  + idle GC
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void initTimers(void);
void exitTimers(void);

void pauseTimers(void);
void unpauseTimers(void);

#include "EndPrivate.h"
