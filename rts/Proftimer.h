/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFTIMER_H
#define PROFTIMER_H

extern void initProfTimer      ( void );
extern void handleProfTick     ( void );

extern void stopProfTimer      ( void );
extern void startProfTimer     ( void );
extern void stopHeapProfTimer  ( void );
extern void startHeapProfTimer ( void );

extern rtsBool performHeapProfile;

#endif /* PROFTIMER_H */
