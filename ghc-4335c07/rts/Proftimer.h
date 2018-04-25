/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void initProfTimer      ( void );
void handleProfTick     ( void );

void stopHeapProfTimer  ( void );
void startHeapProfTimer ( void );

extern bool performHeapProfile;

#include "EndPrivate.h"
