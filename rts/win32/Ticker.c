/*
 * RTS periodic timers.
 * 
 */
#include "Rts.h"
#include "Ticker.h"
#include <windows.h>
#include <stdio.h>
#include <process.h>

/*
 * Provide a timer service for the RTS, periodically
 * notifying it that a number of 'ticks' has passed.
 *
 */

/* To signal pause or shutdown of the timer service, we use a local
 * event which the timer thread listens to.
 */
static HANDLE hStopEvent = INVALID_HANDLE_VALUE;
static HANDLE tickThread = INVALID_HANDLE_VALUE;

static TickProc tickProc = NULL;

static enum { TickerGo, TickerPause, TickerExit } ticker_state;

/*
 * Ticking is done by a separate thread which periodically
 * wakes up to handle a tick.
 *
 * This is the portable way of providing a timer service under
 * Win32; features like waitable timers or timer queues are only
 * supported by a subset of the Win32 platforms (notably not
 * under Win9x.)
 *
 */
static
unsigned
WINAPI
TimerProc(PVOID param)
{
  int ms = (int)param;
  DWORD waitRes = 0;
  
  /* interpret a < 0 timeout period as 'instantaneous' */ 
  if (ms < 0) ms = 0;

  while (1) {
      switch (ticker_state) {
      case TickerGo:
          waitRes = WaitForSingleObject(hStopEvent, ms);
          break;
      case TickerPause:
          waitRes = WaitForSingleObject(hStopEvent, INFINITE);
          break;
      case TickerExit:
          /* event has become signalled */
          tickProc = NULL;
          CloseHandle(hStopEvent);
          hStopEvent = INVALID_HANDLE_VALUE;
          return 0;
      }
      
      switch (waitRes) {
      case WAIT_OBJECT_0:
          /* event has become signalled */
          ResetEvent(hStopEvent);
          continue;
      case WAIT_TIMEOUT:
          /* tick */
          tickProc(0);
          break;
      case WAIT_FAILED:
          sysErrorBelch("TimerProc: WaitForSingleObject failed");
          break; 
      default:
          errorBelch("TimerProc: unexpected result %lu\n", waitRes);
          break;
      }
  }
  return 0;
}


void
initTicker (nat ms, TickProc handle_tick)
{
  unsigned threadId;
  /* 'hStopEvent' is a manual-reset event that's signalled upon
   * shutdown of timer service (=> timer thread.)
   */
  hStopEvent = CreateEvent ( NULL,
			     TRUE,
			     FALSE,
			     NULL);
  if (hStopEvent == INVALID_HANDLE_VALUE) {
      sysErrorBelch("CreateEvent");
      stg_exit(EXIT_FAILURE);
  }
  tickProc = handle_tick;
  ticker_state = TickerPause;
  tickThread = (HANDLE)(long)_beginthreadex( NULL,
			       0,
			       TimerProc,
			       (LPVOID)ms,
			       0,
			       &threadId);

  if (tickThread == 0) {
      sysErrorBelch("_beginthreadex");
      stg_exit(EXIT_FAILURE);
  }
}

void
startTicker(void)
{
    ticker_state = TickerGo;
    SetEvent(hStopEvent);
}

void
stopTicker(void)
{
    ticker_state = TickerPause;
    SetEvent(hStopEvent);
}

void
exitTicker (rtsBool wait)
{
    // We must wait for the ticker thread to terminate, since if we
    // are in a DLL that is about to be unloaded, the ticker thread
    // cannot be allowed to return to a missing DLL.

    if (hStopEvent != INVALID_HANDLE_VALUE && 
	tickThread != INVALID_HANDLE_VALUE) {
	DWORD exitCode;
        ticker_state = TickerExit;
	SetEvent(hStopEvent);
	while (wait) {
            // See #3748:
            //
            // when the RTS is compiled into a DLL (wait==rtsTrue),
            // the ticker thread must stop before we exit, or chaos
            // will ensue.  We can't kill it, because it may be
            // holding a lock.
            //
            // When not compiled into a DLL, we wait for
            // the thread out of courtesy, but give up after 200ms if
            // it still hasn't stopped.
	    WaitForSingleObject(tickThread, 200);
	    if (!GetExitCodeThread(tickThread, &exitCode)) {
		return;
	    }
            CloseHandle(tickThread);
            if (exitCode != STILL_ACTIVE) {
		tickThread = INVALID_HANDLE_VALUE;
		if ( hStopEvent != INVALID_HANDLE_VALUE ) {
		    CloseHandle(hStopEvent);
		    hStopEvent = INVALID_HANDLE_VALUE;
		}
		return;
	    }
	}
    }
}
