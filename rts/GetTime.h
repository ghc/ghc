/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-independent interface to time measurement
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void initializeTimer       (void);

Time getProcessCPUTime     (void);
Time getCurrentThreadCPUTime (void);

/* Get the current date and time.
   Uses seconds since the Unix epoch, plus nanoseconds
 */
void  getUnixEpochTime      (StgWord64 *sec, StgWord32 *nsec);

#include "EndPrivate.h"
