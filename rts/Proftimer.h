/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#pragma once

void initProfTimer      ( void );
void handleProfTick     ( void );
void pauseHeapProfTimer  ( void );
void resumeHeapProfTimer ( void );

extern bool performHeapProfile;
extern bool performTickySample;
