/* -----------------------------------------------------------------------------
 * $Id: Proftimer.h,v 1.2 1998/12/02 13:28:37 simonm Exp $
 *
 * (c) The GHC Team, 1998
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

extern void initProfTimer(nat ms);
extern void stopProfTimer(void);
extern void startProfTimer(void);
extern void handleProfTick(void);

