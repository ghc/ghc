/* -----------------------------------------------------------------------------
 * $Id: Proftimer.h,v 1.5 2000/04/03 15:54:50 simonmar Exp $
 *
 * (c) The GHC Team, 1998
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

extern rtsBool do_prof_ticks;
extern lnat total_prof_ticks;

extern void initProfTimer(nat ms);
extern void stopProfTimer(void);
extern void startProfTimer(void);
extern void handleProfTick(void);
