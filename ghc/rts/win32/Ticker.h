/*
 * RTS periodic timers (win32)
 */
#ifndef __TICKER_H__
#define __TICKER_H__
extern int  startTicker( nat ms, TickProc handle_tick );
extern int  stopTicker ( void );
#endif /* __TICKER_H__ */

