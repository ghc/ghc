/*
 * RTS periodic timers.
 * 
 */
#include "Rts.h"
#include "Timer.h"
#include "Ticker.h"
#include <windows.h>
#include <stdio.h>
#include <process.h>

/*
 * Provide a timer service for the RTS, periodically
 * notifying it that a number of 'ticks' has passed.
 *
 */

/* To signal shutdown of the timer service, we use a local
 * event which the timer thread listens to (and stopVirtTimer()
 * signals.)
 */
static HANDLE hStopEvent = INVALID_HANDLE_VALUE;

static TickProc tickProc = NULL;

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
  DWORD waitRes;
  
  /* interpret a < 0 timeout period as 'instantaneous' */
  if (ms < 0) ms = 0;

  while (1) {
    waitRes = WaitForSingleObject(hStopEvent, ms);
    
    switch (waitRes) {
    case WAIT_OBJECT_0:
      /* event has become signalled */
      tickProc = NULL;
      CloseHandle(hStopEvent);
      return 0;
    case WAIT_TIMEOUT:
      /* tick */
      tickProc(0);
      break;
    case WAIT_FAILED: {
	DWORD dw = GetLastError();
	fprintf(stderr, "TimerProc: wait failed -- error code: %lu\n", dw); fflush(stderr);
	break; 
    }
    default:
      fprintf(stderr, "TimerProc: unexpected result %lu\n", waitRes); fflush(stderr);
      break;
    }
  }
  return 0;
}


int
startTicker(nat ms, TickProc handle_tick)
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
    return 0;
  }
  tickProc = handle_tick;
  return ( 0 != _beginthreadex(NULL,
			       0,
			       TimerProc,
			       (LPVOID)ms,
			       0,
			       &threadId) );
}

int
stopTicker(void)
{
  if (hStopEvent != INVALID_HANDLE_VALUE) {
    SetEvent(hStopEvent);
  }
  return 0;
}
