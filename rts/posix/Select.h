/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Prototypes for functions in Select.c
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

// An absolute time value in units of 10ms.
typedef StgWord LowResTime;

LowResTime getDelayTarget (HsInt us);

void awaitCompletedTimeoutsOrIOSelect(Capability *cap, bool wait);

#include "EndPrivate.h"

