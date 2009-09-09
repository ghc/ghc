/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFTIMER_H
#define PROFTIMER_H

BEGIN_RTS_PRIVATE

void initProfTimer      ( void );
void handleProfTick     ( void );

#ifdef PROFILING
void stopProfTimer      ( void );
void startProfTimer     ( void );
#endif

void stopHeapProfTimer  ( void );
void startHeapProfTimer ( void );

extern rtsBool performHeapProfile;

END_RTS_PRIVATE

#endif /* PROFTIMER_H */
