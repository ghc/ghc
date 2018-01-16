/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Prototypes for functions in Select.c
 *
 * -------------------------------------------------------------------------*/

#pragma once

// An absolute time value in units of 10ms.
typedef StgWord LowResTime;

RTS_PRIVATE LowResTime getDelayTarget (HsInt us);
