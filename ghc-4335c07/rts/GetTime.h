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
void getProcessTimes       (Time *user, Time *elapsed);

/* Get the current date and time.
   Uses seconds since the Unix epoch, plus nanoseconds
 */
void  getUnixEpochTime      (StgWord64 *sec, StgWord32 *nsec);

// Not strictly timing, but related
W_    getPageFaults         (void);

#include "EndPrivate.h"
