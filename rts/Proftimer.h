/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFTIMER_H
#define PROFTIMER_H

void stopProfTimer      ( void );
void startProfTimer     ( void );

#include "BeginPrivate.h"

void initProfTimer      ( void );
void handleProfTick     ( void );

void stopHeapProfTimer  ( void );
void startHeapProfTimer ( void );

extern rtsBool performHeapProfile;

#include "EndPrivate.h"

#endif /* PROFTIMER_H */
