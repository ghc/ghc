/* -----------------------------------------------------------------------------
 * $Id: Proftimer.h,v 1.6 2001/11/22 14:25:12 simonmar Exp $
 *
 * (c) The GHC Team, 1998
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

extern void initProfTimer      ( void );
extern void handleProfTick     ( void );

extern void stopProfTimer      ( void );
extern void startProfTimer     ( void );
extern void stopHeapProfTimer  ( void );
extern void startHeapProfTimer ( void );

extern rtsBool performHeapProfile;
