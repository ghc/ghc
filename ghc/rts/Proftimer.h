/* -----------------------------------------------------------------------------
 * $Id: Proftimer.h,v 1.4 1999/08/25 16:11:50 simonmar Exp $
 *
 * (c) The GHC Team, 1998
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

extern lnat total_prof_ticks;

extern void initProfTimer(nat ms);
extern void stopProfTimer(void);
extern void startProfTimer(void);
extern void handleProfTick(int unused);

