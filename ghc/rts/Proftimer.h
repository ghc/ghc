/* -----------------------------------------------------------------------------
 * $Id: Proftimer.h,v 1.3 1999/08/04 17:03:41 panne Exp $
 *
 * (c) The GHC Team, 1998
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

extern void initProfTimer(nat ms);
extern void stopProfTimer(void);
extern void startProfTimer(void);
extern void handleProfTick(int unused);

