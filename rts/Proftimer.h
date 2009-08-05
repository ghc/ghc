/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFTIMER_H
#define PROFTIMER_H

#pragma GCC visibility push(hidden)

void initProfTimer      ( void );
void handleProfTick     ( void );

#ifdef PROFILING
void stopProfTimer      ( void );
void startProfTimer     ( void );
#endif

void stopHeapProfTimer  ( void );
void startHeapProfTimer ( void );

extern rtsBool performHeapProfile;

#pragma GCC visibility pop

#endif /* PROFTIMER_H */
