/*
 * RTS periodic timers.
 *
 * http://undocumented.ntinternals.net/index.html?page=UserMode%2FUndocumented%20Functions%2FTime%2FNtSetTimerResolution.html
 * https://docs.microsoft.com/en-us/windows/desktop/SysInfo/acquiring-high-resolution-time-stamps#hardware-timer-info
 *
 */

#include "Rts.h"
#include "Ticker.h"
#include <windows.h>
#include <stdio.h>
#include <process.h>
#include <winnt.h>
#include <ntdef.h>
#include <ntstatus.h>
#include <stdbool.h>

/* These functions are undocumented Windows APIs that are most commonly used by
   driver code.  They're the building blocks for the Win32 API functions and
   haven't changed since David Cutler proposed them to the Portable System's
   group in 1989.  So they should be pretty safe.  They also provise a
   resolution of 100ns, which is much better than the 15ms of the normal Win32
   API calls.  */

typedef struct _TIMER_BASIC_INFORMATION {
  LARGE_INTEGER           RemainingTime;
  BOOLEAN                 TimerState;
} TIMER_BASIC_INFORMATION, *PTIMER_BASIC_INFORMATION;

typedef enum _TIMER_INFORMATION_CLASS {
  TimerBasicInformation
} TIMER_INFORMATION_CLASS, *PTIMER_INFORMATION_CLASS;

typedef void (WINAPI * PTIMER_APC_ROUTINE)(LPVOID TimerContext, DWORD TimerLowValue, DWORD TimerHighValue);

typedef NTSTATUS (WINAPI *NtCreateTimerCB         ) (PHANDLE, ACCESS_MASK, POBJECT_ATTRIBUTES, TIMER_TYPE);
typedef NTSTATUS (WINAPI *NtCancelTimerCB         ) (HANDLE, PBOOLEAN);
typedef NTSTATUS (WINAPI *NtQueryTimerCB          ) (HANDLE, TIMER_INFORMATION_CLASS, PVOID, ULONG, PULONG);
typedef NTSTATUS (WINAPI *NtSetTimerCB            ) (HANDLE, PLARGE_INTEGER, PTIMER_APC_ROUTINE, PVOID, BOOLEAN, LONG, PBOOLEAN);
typedef NTSTATUS (WINAPI *NtOpenTimerCB           ) (PHANDLE, ACCESS_MASK, POBJECT_ATTRIBUTES);
typedef ULONG    (WINAPI *NtGetTickCountCB        ) (VOID);
typedef NTSTATUS (WINAPI *NtQueryTimerResolutionCB) (PULONG, PULONG, PULONG);
typedef NTSTATUS (WINAPI *NtSetTimerResolutionCB  ) (ULONG, BOOLEAN, PULONG);
typedef NTSTATUS (WINAPI *NtCloseCB               ) (HANDLE);

static NtCreateTimerCB          NtCreateTimer;
static NtCancelTimerCB          NtCancelTimer;
static NtQueryTimerCB           NtQueryTimer;
static NtSetTimerCB             NtSetTimer;
static NtOpenTimerCB            NtOpenTimer;
static NtGetTickCountCB         NtGetTickCount;
static NtQueryTimerResolutionCB NtQueryTimerResolution;
static NtSetTimerResolutionCB   NtSetTimerResolution;
static NtCloseCB                NtClose;
static HMODULE ntdll;
/* Determine if the kernel interface for the timer is usable.  If so use it.  */
static volatile boolean use_kernel_timer = false;
static volatile boolean timers_running = false;

/* Timer Queue fallback code.  */
static TickProc tick_proc = NULL;
static HANDLE timer_queue = NULL;
static HANDLE timer       = NULL;
static Time tick_interval = 0;
static LARGE_INTEGER timeout;
static LARGE_INTEGER Frequency;
static LARGE_INTEGER StartingTime, EndingTime, ElapsedMicroseconds;

static void WINAPI
kernel_callback (
  LPVOID TimerContext STG_UNUSED,
  DWORD TimerLowValue STG_UNUSED,
  DWORD TimerHighValue STG_UNUSED)
{
  QueryPerformanceCounter(&EndingTime);
  tick_proc(0);

  ElapsedMicroseconds.QuadPart = EndingTime.QuadPart - StartingTime.QuadPart;
  ElapsedMicroseconds.QuadPart *= 1000000;
  ElapsedMicroseconds.QuadPart /= Frequency.QuadPart;
  debugBelch ("%llu\n", ElapsedMicroseconds.QuadPart);
}

static void WINAPI
kernel_callback_test (
  LPVOID TimerContext STG_UNUSED,
  DWORD TimerLowValue STG_UNUSED,
  DWORD TimerHighValue STG_UNUSED)
{
  use_kernel_timer = true;
}

static VOID CALLBACK tick_callback(
  PVOID lpParameter STG_UNUSED,
  BOOLEAN TimerOrWaitFired STG_UNUSED)
{
  tick_proc(0);
}

static DWORD WINAPI timer_manager (LPVOID lpParameter STG_UNUSED)
{
  while (timers_running) {
    /* Set thread into an alertable state.  */
    DWORD res = WaitForSingleObjectEx (timer, INFINITE, true);
    if (res == WAIT_OBJECT_0)
      tick_proc (0);

    TIMER_BASIC_INFORMATION info;
    /* Do we really need this call? May cause a drift.. probably checking return
       for the wait is enough.  */
    NtQueryTimer (timer, TimerBasicInformation, &info, sizeof(info), NULL);

    /* If signaled reset the timer, since the interval for automatic timer reset
       is in the 100ms range and so it's useless.  */
    if (info.TimerState) {
      NTSTATUS result = NtSetTimer (timer, &timeout, &kernel_callback, NULL,
                                    0, 0, NULL);
      QueryPerformanceCounter(&StartingTime);
      /* Error handling here may be a bit harsh.. perhaps emit a non-fatal error
         instead?.  */
      if (result != STATUS_SUCCESS) {
        sysErrorBelch("NtSetTimer");
        stg_exit(EXIT_FAILURE);
      }
    }
  }
  return 0;
}

// We use the CreateTimerQueue() API which has been around since
// Windows 2000.  Apparently it gives bad results before Windows 7,
// though: http://www.virtualdub.org/blog/pivot/entry.php?id=272
//
// Even with the improvements in Windows 7, this timer isn't going to
// be very useful for profiling with a max usable resolution of
// 15ms. Unfortunately we don't have anything better. <-- We do (Phyx).


// Update as of 2020-04-02:
// It seems we can get somewhat reliable resolution even for intervals
// at 1ms which had an average error of <5%.
// This seems to be the case starting at some point during the
// Windows 7 lifetime and any newer versions of windows.

void
initTicker (Time interval, TickProc handle_tick)
{
  tick_interval = interval;
  tick_proc     = handle_tick;

  ntdll                  = LoadLibraryW (L"ntdll.dll");
  NtCreateTimer          = (NtCreateTimerCB)         GetProcAddress (ntdll, "NtCreateTimer");
  NtCancelTimer          = (NtCancelTimerCB)         GetProcAddress (ntdll, "NtCancelTimer");
  NtQueryTimer           = (NtQueryTimerCB)          GetProcAddress (ntdll, "NtQueryTimer");
  NtSetTimer             = (NtSetTimerCB)            GetProcAddress (ntdll, "NtSetTimer");
  NtOpenTimer            = (NtOpenTimerCB)           GetProcAddress (ntdll, "NtOpenTimer");
  NtGetTickCount         = (NtGetTickCountCB)        GetProcAddress (ntdll, "NtGetTickCount");
  NtQueryTimerResolution = (NtQueryTimerResolutionCB)GetProcAddress (ntdll, "NtQueryTimerResolution");
  NtSetTimerResolution   = (NtSetTimerResolutionCB)  GetProcAddress (ntdll, "NtSetTimerResolution");
  NtClose                = (NtCloseCB)               GetProcAddress (ntdll, "NtClose");

  HANDLE detectTimer;
  NTSTATUS result;
  IF_DEBUG(scheduler, debugBelch ("* Detecting kernel support for high accuracy timers..\n"));
  result = NtCreateTimer (&detectTimer, TIMER_ALL_ACCESS, NULL, SynchronizationEvent);
  IF_DEBUG(scheduler, debugBelch ("* Detecting NtCreateTimer support..\n"));
  if (result == STATUS_SUCCESS) {
    timeout.QuadPart = -1000LL;
    result = NtSetTimer (detectTimer, &timeout, &kernel_callback_test, NULL, false, 0, NULL);
    IF_DEBUG(scheduler, debugBelch ("* Detecting NtSetTimer support..\n"));
    if (result == STATUS_SUCCESS) {
      DWORD res = WaitForSingleObjectEx (detectTimer, 100, true);
      use_kernel_timer |= res == WAIT_OBJECT_0;
      NtClose (detectTimer);
    }
    IF_DEBUG(scheduler, debugBelch ("* NtSetTimer signaled: %d.\n", use_kernel_timer));
    if (use_kernel_timer) {
      timers_running = true;
      timer_queue = CreateThread (NULL, 0, &timer_manager, NULL, CREATE_SUSPENDED, NULL);
      if (!timer_queue) {
        use_kernel_timer = false;
      }

      if (use_kernel_timer) {
        IF_DEBUG(scheduler, debugBelch ("* Kernel supports high accuracy timers. Enabling..\n"));
        return;
      }
    };
    use_kernel_timer = false; // disable for now.
  }

  if (!use_kernel_timer) {
    timer_queue = CreateTimerQueue();
    if (timer_queue == NULL) {
        sysErrorBelch("CreateTimerQueue");
        stg_exit(EXIT_FAILURE);
    }
  }
}

void
startTicker(void)
{
  char* errmsg;
  //debugBelch ("* Starting ticker..\n");
  if (use_kernel_timer) {
    //debugBelch ("* Starting timer for: %lluns\n", TimeToNS (tick_interval))
    NTSTATUS result = NtCreateTimer (&timer, TIMER_ALL_ACCESS, NULL, SynchronizationEvent);
    if (result != STATUS_SUCCESS) { errmsg = "NtCreateTimer"; goto fail; }

    timeout.QuadPart = -(TimeToNS (tick_interval) / 100);
    result = NtSetTimer (timer, &timeout, &kernel_callback, NULL, 0, 0, NULL);
    if (result != STATUS_SUCCESS) { errmsg = "NtSetTimer"; goto fail; }
    timers_running = true;
    QueryPerformanceFrequency(&Frequency);
    if (ResumeThread(timer_queue) == (DWORD)-1) { errmsg = "ResumeThread"; goto fail; }
    QueryPerformanceCounter(&StartingTime);
    //debugBelch ("* Timer started successfully.\n");
  } else {
    BOOL r;

    r = CreateTimerQueueTimer(&timer,
                              timer_queue,
                              tick_callback,
                              0,
                              0,
                              TimeToMS(tick_interval), // ms
                              WT_EXECUTEINTIMERTHREAD);
    if (r == 0) {
        sysErrorBelch("CreateTimerQueueTimer");
        stg_exit(EXIT_FAILURE);
    }
  }
  return;

fail:
  sysErrorBelch(errmsg);
  stg_exit(EXIT_FAILURE);
}

void
stopTicker(void)
{
  if (timer_queue != NULL && timer != NULL) {
    if (use_kernel_timer) {
      SuspendThread (timer_queue);
      NtCancelTimer (timer, NULL);
      CloseHandle (timer);
      //debugBelch ("* Timer stopped.\n");
    } else {
      DeleteTimerQueueTimer(timer_queue, timer, NULL);
    }
    timer = NULL;
  }
}

void
exitTicker (bool wait)
{
  stopTicker();
  if (use_kernel_timer) {
    timers_running = false;
    CloseHandle (timer_queue);
  } else if (timer_queue != NULL) {
    DeleteTimerQueueEx(timer_queue, wait ? INVALID_HANDLE_VALUE : NULL);
  }
  timer_queue = NULL;
}
